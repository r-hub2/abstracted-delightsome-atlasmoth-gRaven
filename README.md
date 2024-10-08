# gRaven
<img align="right" height="260" src="https://github.com/petergreen5678/images/blob/master/raven.jpg">
Wrappers for functions in the 'gRain' package to emulate some 'RHugin' functionality, allowing the building of Bayesian networks consisting of discrete chance nodes incrementally, through adding nodes, edges and conditional probability tables, the setting of evidence, whether 'hard' (boolean) or 'soft' (likelihoods), and querying marginal probabilities and normalizing constants. Computations will typically not be so fast as they are with 'RHugin', but this package should assist users without access to 'Hugin' to use code written to use 'RHugin'. I am grateful to Therese Graversen for her contributions, especially the function map.configurations, and to Søren Højsgaard for enhancements to his gRain package that improve the performance of this package.

To install, type remotes::install_github("petergreen5678/gRaven")
