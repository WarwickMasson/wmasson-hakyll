---
title: Visualizing Markov Chains
date: January 17, 2013
tags: d3, markov chains
---
Markov chains are used for modelling processes that have a number of possible states and at each time step move from the current state to the next one
based on some transition probabilities. The collection of transition probabilities is given in the form of a transition matrix.
 For example, suppose we have 2 weather states: rainy and sunny. Then if the transition probabilities are given by

    rainy to sunny: 70%
    rainy to rainy: 30%
    sunny to rainy: 5%
    sunny to sunny: 95%

So the transition matrix is then

            sunny   rainy
    sunny    0.95    0.05
    rainy    0.7     0.3

A markov chain also has a set of initial probabilities - how likely it is to start in a given state. For example the probabilities of starting a new season
may be

    sunny 90%
    rainy 10%

It's easy to imagine how this chains works - a large number of sunny days with occasional rainy days and rarer rainy periods. Imagining how larger markov
chains operate can be difficult when we have for example 10 states and consequently a 10 by 10 transition matrix. One way is follow an instances of the
chain as it moves from state to state. This gives us some idea, but it can still be hard to see exactly how the probabilities work. Rather, let's follow
a group of instances. 

This animation simulates a large number of ants moving from nest to nest. When the ant arrives at a next nest, it moves to another nest based
on a randomly generated transition matrix. I've biased the transition matrix so that it appears more uneven and added some noise to the speed of 
the ants so that they don't just set off at the same time. 

<iframe style="border: 0px;" scrolling="no" width="500px" height="500px"
src = "../js/markov_chain.html"></iframe>

Notice how certain paths are very well travelled and others only have intermittent ants. The number of ants of a path from nest A to nest B
is not just dependent on probability of going from A to B but also how many ants are going to A in the first place. This allows us to see the general
traffic flow in the chain. 
