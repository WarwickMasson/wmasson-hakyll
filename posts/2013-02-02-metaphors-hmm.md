---
title: Metaphors for Hidden Markov Models
date: February 2, 2013
tags: hidden markov models, machine learning
---

Hidden Markov Models are complex mathematical structures that allow for a wide range of intuitive notions.
I'm going to try explain my intuition behind them but first let's look at their exact definition.

A hidden markov model consists of a number of states N, a number of symbols K, an NxN transition probability matrix A, an NxK emission probability matrix B,
and a initial probability vector PI. We have a set of time steps t, at each time step we are in a particular state and emit a symbol. At the next time step
we change state based on the transition probability. We can formulate this as

	Q(t) = the state at time t
	O(t) = the symbol emitted at time t
	A(i,j) = Probability we are in state i move to state j next
	B(j,k) = Probability we are in state j and it emits symbol k
	PI(i) = Probability we are in state i at time step 1
    O(t) = the observation at time t

Hidden markov models have the markov property which is to say those probabilities above only depend on the current state. Let's give an example of a hidden markov model
 (hereafter HMM). HMMs have hidden or unobservable state. They are useful when we have a process that generates a sequence of symbols
but we can't see the process itself. So let's give an example of one much situation.

### A Dungeon
Imagine you are a prisoner in a dank, dark dungeon. Everyday a guard comes in to give you some gruel. A simple man, he is either happy or sad. The guard's emotions are
dependent on the weather outside, which is equally simplistic - it's either rainy or sunny and today's weather only depends on the previous day's weather.
Also you know some facts about both the weather and the guard. These facts are:

	4 times out of 10, when it rains, it rains again the next day
	8 times out of 10, when it is sunny, it is sunny again the next day
	6 times out of 10, when it rains, the guard is sad
	9 times out of 10, when it is sunny, the guard is happy
	When you started your incarceration, it was sunny

This situation describes a HMM so how do we decide what is what? First of all the states in our HMM correspond to the weather - they are unobservable and the symbols are the emotions of the guard because they are dependent on the state and they are observable. In this case our N is 2 and our K is 2.
What are the transition probabilities? 

	A(sunny,sunny) = 0.8
	A(sunny,rainy) = 0.2
	A(rainy,rainy) = 0.4
	A(rainy,sunny) = 0.6

What about the emission probabilities?

	B(sunny,happy) = 0.9
	B(sunny,sad) = 0.1
	B(rainy,happy) = 0.4
	B(rainy,sad) = 0.6

And the initial probabilities?

	PI(sunny) = 1
	PI(rainy) = 0

This is because so far we have only started in the sunny state.

### Two Guards

Suppose there are two guards in the prison and their moods are dependent are different things. It's too dark in too determine which guard is guarding our cell 
but we can figure out the guard's mood based on how they slam the gate. By listening in closely we can get a list of moods:

    happy, sad, happy, happy, sad, happy, sad, sad
    
What we'd like to do is work out which of the guards is guarding the cell. We can do this by assigning a probability to each guard based on how likely that guard is
to exhibit the given list of moods. We can work out this probability using the forward algorithm. We can the list of moods an observation sequence and denote it by
O(1),O(2),...,O(T) where T is number of moods observed. We then define Forward(t,s) as the probability of seeing the sequence O(1),...,O(t) given that we are in state s 
at time t. To calculate Forward(t,s) we use the following recursive formulae:

    Forward(1,s) = B(s,O(1)) * PI(s)
    Forward(t,s) = B(s,O(t)) * (sum for i=1,...,n of A(i,s) * Forward(t-1,i))

Forward(1,s) is the probability of seeing just the first mood given we are in state s. This is just the probability of emitting the first mood and starting in state
s. The second equation is trickier. To work out Forward(t,s) it's the probability we emit mood t if we're in state s combined the probability we are in state s in time
t. That probability is sum of the routes to state s - for the previous step what's the probability we were in state i and then transitioned to state s. We continue in
this way until we know Forward(T,s) for all s.

By our definition Forward(T,s) is the probability of seeing the whole sequence and being in state s at the end. If we find the sum of Forward(T,s) for all s then we
will know the probability of seeing the whole sequence. For each guard we compute the probability they produced the given list of moods and then we can conclude that
the guard with the greatest probability is the one who is guarding us. 

### Learning the guard

In the previous example we somehow knew how the guard works - we know both what causes the guard to be happy or sad and how the weather changes. These things together
constitute a model of the guard.  Now suppose you locked in an underground dungeon with a new guard. As we want to know how our guard thinks and works we want to come up with
a model. 

We do this using the Baum Welch algorithm. This takes in a set of observations and creates a model that would explain those observations. We get a
list of observations or even a list of observation sequences and use the Baum Welch algorithm to train the model.

To begin with we start with a new HMM and initialize it, usually with random parameters. To evaluate how well the model works at explaining we can use the forward
algorithm and feed in our training data. The Baum Welch algorithm takes in a model and a set of observations and gives us an updated model that is more likely
to produce those observations. This new model is then used in the Baum Welch algorithm again to get an even better model. This keeps going until our model
converges to some state where it will be likely to produce the training data.

Once we create a satisfactory model we can use it for recognizing guards as we did in the previous section. An important thing about choosing our initial model is
the number of states. Previously we knew what the states represented in the real world. Now the states are only representing the internal process inside the guard
that generates the given moods. This might be the weather, the local sports games or something internal. Since we don't know what the states represent
we won't necessarily know what the best number of states is to model the guard. It might be worth experimenting with different numbers of states until we find
a maximum. 

### Resources

The [Rabiner Tutorial](http://www.cs.cornell.edu/Courses/cs4758/2012sp/materials/hmm_paper_rabiner.pdf) on hidden markov models is a landmark paper on the
topic and gives a complete look at hidden markov models and their use in speech recognition. Ali Rahimi provides some [errata](http://alumni.media.mit.edu/~rahimi/rabiner/rabiner-errata.html) for the Rabiner paper that are worth looking at if you are planning on implementing the algorithms yourself. 

Jonathan C. Hall gives a useful tutorial on [recognizing gestures](http://www.creativedistraction.com/demos/gesture-recognition-kinect-with-hidden-markov-models-hmms/) using the Kinect with HMM.
