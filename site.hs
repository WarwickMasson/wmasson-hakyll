{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>))
import Data.List (intercalate,sortBy)
import Text.Blaze.Html (toHtml,toValue, (!))
import Text.Blaze.Html.Renderer.String (renderHtml)
import qualified Text.Blaze.Html5 as BlazeHtml
import qualified Text.Blaze.Html5.Attributes as BlazeAttr
import Data.Monoid (mappend, mconcat)
import Hakyll.Web.Tags (renderTags)
import Control.Monad (forM)
import Data.Maybe (fromMaybe)
import Hakyll

main :: IO ()
main = hakyll $ do
    -- Static files
    match ("images/*" .||. "js/*" .||. "favicon.ico" .||. "CNAME" .||. "files/*") $ do
        route idRoute
        compile copyFileCompiler

    -- Compress CSS
    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    -- About
    match "about.md" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/about.html" defaultContext
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Build Tags
    tags <- buildTags "posts/*" (fromCapture "tags/*.html")

    -- Individual Posts
    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" (postContext tags)
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    -- Posts
    create ["posts.html"] $ do
        route idRoute
        compile $ do
            list <- postList tags "posts/*" recentFirst
            let postsContext = constField "posts" list `mappend`
                    constField "title" "Posts" `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" postsContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls
    
    -- Index
    create ["index.html"] $ do
        route idRoute
        compile $ do
            list <- postListNum tags "posts/*" recentFirst 3
            let indexContext = constField "posts" list `mappend`
                    constField "title" "Home" `mappend`
                    defaultContext
            
            makeItem ""
                >>= loadAndApplyTemplate "templates/index.html" indexContext
                >>= loadAndApplyTemplate "templates/default.html" indexContext
                >>= relativizeUrls
    
    -- 404
    create ["404.html"] $ do
        route idRoute
        compile $ do
            let notFoundContext = constField "title" "404 - Not Found" `mappend`
                    constField "body" "The page you were looking for was not found." `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/about.html" notFoundContext 
                >>= loadAndApplyTemplate "templates/default.html" notFoundContext
                >>= relativizeUrls

    -- Tag Pages
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged with " ++ tag

        route idRoute
        compile $ do
            list <- postList tags pattern recentFirst
            let tagContext = constField "title" title `mappend`
                    constField "posts" list `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/posts.html" tagContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls 

    -- Tags
    create ["tags.html"] $ do
        route idRoute
        compile $ do
            tagList <- renderTagElem tags 
            let tagsContext = constField "title" "Tags" `mappend`
                    constField "tags" tagList `mappend`
                    defaultContext
            makeItem ""
                >>= loadAndApplyTemplate "templates/tags.html" tagsContext
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls 

    -- RSS
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            posts <- loadAll "posts/*"
            sorted <- take 10 <$> recentFirst posts
            renderRss feedConfiguration feedContext sorted

    match "templates/*" $ compile templateCompiler


------------------------------------------------------

postContext :: Tags -> Context String
postContext tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%B %e, %Y"
    , tagsField "tags" tags
    , defaultContext
    ]

------------------------------------------------------

postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String]) -> Compiler String
postList tags pattern prep = do
    posts <- loadAll pattern
    itemTemplate <- loadBody "templates/postitem.html"
    processed <- prep posts
    applyTemplateList itemTemplate (postContext tags) processed

postListNum :: Tags -> Pattern -> ([Item String] -> Compiler [Item String]) -> Int -> Compiler String
postListNum tags pattern prep num = do
    posts <- prep <$> loadAll pattern
    itemTemplate <- loadBody "templates/postitem.html"
    taken <- take num <$> posts
    applyTemplateList itemTemplate (postContext tags) taken

-----------------------------------------------------

renderTagElem :: Tags -> Compiler String
renderTagElem = renderTags makeLink (intercalate "\n")
    where
        makeLink tag url count _ _ = renderHtml $
            BlazeHtml.li $ BlazeHtml.a ! BlazeAttr.href (toValue url) $ toHtml (tag ++ " - " ++ show count ++(postName count))

postName :: Int -> String
postName 1 = " post"
postName _ = " posts"

------------------------------------------------------

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "warwickmasson.com - Posts"
    , feedDescription = "Posts from warwickmasson.com"
    , feedAuthorName = "Warwick Masson"
    , feedAuthorEmail = "warwick.masson@gmail.com"
    , feedRoot = "http://warwickmasson.com"
    }

------------------------------------------------------

feedContext :: Context String
feedContext = constField "description" "" `mappend` defaultContext
