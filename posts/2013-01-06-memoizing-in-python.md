---
title: Memoizing Functions in Python
date: January 6, 2013
tags: python, functional programming
---

Memoization is the process of storing the return values of a function so that if the function is called again with the same arguments, we can return the stored value
instead of recomputing it. Suppose we have a function f that is costly to run. We can memoize the return values by calling an intermediary

~~~~~{.python}
def f(x):
	# some expensive computation

seen = {} 

def memoize_f(x):
	if x in seen:
		return seen[x]
	else:
		seen[x] = f(x)
		return seen[x]
~~~~~

If we want to call f, instead we call memoize_f and it will remember the values. This is inconvenient for two reasons. If we have another function, say g,
that needs memoization we need another function memoize_g. Also, the seen list is globally accessible which is undesirable. Instead we use a wrapper class for the
function to handle the memoization.

~~~~~{.python}
class memoizer:
	def __init__(self,f):
		self.seen = {}
		self.f = f

	def lookup(self,x):
		if x in self.seen:
			return self.seen[x]
		else:
			self.seen[x] = self.f(x)
			return self.seen[x]

memoize_f = memoizer(f)
~~~~~

This is a cleaner solution. When we want to get the value of f(x) we can call memoize_f.lookup(x). This is somewhat inelegant, ideally we should just have memoize_f
be a function itself.

~~~~~{.python}	
def memoize_func(f):
	m = memoizer(f)
	return m.lookup

memoize_f = memoize_func(f)
~~~~~

This will return the part we want - the lookup function. This has the added bonus of obscuring the class itself: instead we just have the function that acts like
f and we don't have to worry about how the memoization works. There's another little trick to make this even better.

	memoize = memoize_func(memoize_func)

Here we have memoized the memoize_func so we always get back the same memoized version of a function.
Suppose we want the memoized version of f but don't have it in scope, then if we can just call memoize(f) and get the same memoized version we had before. For one final addition, let's make it support multiple arguments:

~~~~~{.python}
class memoizer:	
	def __init__(self,f):
		self.seen = {}
		self.f = f

	def lookup(self,*x):
		if x in self.seen:
			return self.seen[x]
		else:
			self.seen[x] = self.f(*x)
			return self.seen[x]
~~~~~

This uses python's *args feature to get a list of arguments.
