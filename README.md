# Intro-To-ML
This repository contains files containing attempts at solutions to problems for lab sessions part of a lecture series prepared for a 2-week introductory course to machine learning by Etienne A.D Pienaar.  
The content of the lecture series by Etienne A.D. Pienaar is licensed under CC BY-NC-ND 4.0.

### Repository Details
Each directory contains a compiled pdf of the RMarkdown files for the lab. In addition to this the R Script files are included in the same directories.

## Lab 1

### Regression Trees
* Simulate a non-linear (reg.) pattern, apply the partitionin algorithm to the pattern for various stopping criteria, and draw response curves to demonstrate model complexity.
### Classification Trees
* Use a tree-based model to analyse the iris data in R. Plot the fitted trees and interpret. (Fit and) Draw a response curve for the Petal-inputs and superimpose the observations on the response curve.

## Lab 2

### Goals
* Fit a tree model to the simulated dataset, applying the appropriate complexity controls. Compare the resulting response curve to one fitted using an unconstrained tree.
* Analyse the ptitanic data from the rpart.plot package using an appropriately chosen tree-based model.

## Lab 3

### Goals
* Analyse the iris data using the neuralnet package (no validation, just use all the data) and evaluate the response curve under a neural network.


### Extra Problems
* Problem 1: For the iris data, what activation function would you specify for the output layer that would be more appropriate?
* Problem 2: Re-run the analysis (all else equal) using ‘learning rate=0.01’.
* Problem 3: Early stopping is said to be a form of regularisation. Re-run the analysis (all else equal) using learning-rate = 0.01 and threshold = 0.1. Do you concur that the resulting fitted model is simpler than what you would’ve observed under less stringent stopping criterion?
