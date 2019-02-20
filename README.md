# RStan-package-test
Test repository for my first precompiled RStan package.

# Installation
Use devtools to install the package directly from the repository. The package
needs to compile the models in C++ so a compiler is required.

You may have trouble installing the 32-bit and 64-bit versions on Windows. In 
that case only install one version with

which will automatically install arch that is used in your current RStudio
session. If it still does not install, you may need to check your system Path,
where the compiler for the desired version should be before the compiler for
the other version.

# Use
The main functionality of the package is training and predicting with
different factor analysis methods. For that, you only need to use functions
starting with train_ and pred_. Inputs to functions are very similar, 
for details see documentation. Additionally there are four sports count data 
sets that can be used for the training and evaluation. The data are from the
English association football Premier League (EPL), National basketball league
(NBA), National Football League (NFL), and esports League of Legends Champions
Korea (LCK).

For example, to train, predict, and evaluate
the copula negative binomial factor analysis method on EPL data, use
