{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Arrow ((>>>), (***), arr)
import Control.Category (id)
import Data.Monoid (mempty, mconcat)

import Hakyll

main :: IO ()
main = hakyll $ do
    -- Compress CSS
    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    -- Copy Images
    match "images/*" $ do
	route   idRoute
        compile copyFileCompiler

    -- Render posts
    match "posts/*" $ do
        route   $ setExtension ".html"
        compile $ pageCompiler
            >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
            >>> renderTagsField "prettytags" (fromCapture "tags/*")
            >>> applyTemplateCompiler "templates/post.html"
            >>> arr (copyBodyToField "description")
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler

    -- Render posts list
    match "posts.html" $ route idRoute
    create "posts.html" $ constA mempty
        >>> arr (setField "title" "Posts")
        >>> requireAllA "posts/*" addPostList
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Index
    match "index.html" $ route idRoute
    create "index.html" $ constA mempty
        >>> arr (setField "title" "Home")
        >>> requireAllA "posts/*" (id *** arr (take 3 . reverse . chronological) >>> addPostList)
        >>> applyTemplateCompiler "templates/index.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

    -- Tags

    create "tags" $
        requireAll "posts/*" (\_ ps -> readTags ps :: Tags String)
    -- Render tags list
    match "tags.html" $ route idRoute
    create "tags.html" $ constA mempty
	>>> arr (setField "title" "Tags")
	>>> requireA "tags" (setFieldA "tagcloud" (renderTagCloud'))
	>>> applyTemplateCompiler "templates/tags.html"
	>>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler	

    -- Add a tag list compiler for every tag
    match "tags/*" $ route $ setExtension ".html"
    metaCompile $ require_ "tags"
        >>> arr tagsMap
        >>> arr (map (\(t, p) -> (tagIdentifier t, makeTagList t p)))
    
    -- About
    match "about.md" $ do
        route $ setExtension "html"
        compile $ pageCompiler
	    >>> applyTemplateCompiler "templates/about.html"
            >>> applyTemplateCompiler "templates/default.html"
            >>> relativizeUrlsCompiler
    -- Favicon
    match "favicon.ico" $ do
	route   idRoute
        compile copyFileCompiler

    -- Render RSS feed
    match "rss.xml" $ route idRoute
    create "rss.xml" $
        requireAll_ "posts/*"
            >>> renderRss feedConfiguration

    -- Read templates
    match "templates/*" $ compile templateCompiler
  where
    renderTagCloud' :: Compiler (Tags String) String
    renderTagCloud' = renderTagCloud tagIdentifier 100 300

    renderTagList' :: Compiler (Tags String) String
    renderTagList' = renderTagList tagIdentifier

    tagIdentifier :: String -> Identifier (Page String)
    tagIdentifier = fromCapture "tags/*"

-- | Auxiliary compiler: generate a post list from a list of given posts, and
-- add it to the current page under @$posts@
--
addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/postitem.html" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

makeTagList :: String
            -> [Page String]
            -> Compiler () (Page String)
makeTagList tag posts =
    constA (mempty, posts)
        >>> addPostList
        >>> arr (setField "title" ("Posts tagged &#8216;" ++ tag ++ "&#8217;"))
        >>> applyTemplateCompiler "templates/posts.html"
        >>> applyTemplateCompiler "templates/default.html"
        >>> relativizeUrlsCompiler

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle       = "wmasson RSS feed."
    , feedDescription = "wmasson posts"
    , feedAuthorName  = "Warwick Masson"
    , feedAuthorEmail = "warwick.masson@gmail.com"
    , feedRoot        = ""
    }
