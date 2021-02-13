## What is Rgogo?

Rgogo is a software framework for actuarial modeling.  It is an R package comprising a collection of classes and tools that are designed for actuarial modeling.  These classes and tools are building blocks that allow actuaries to build and run complex actuarial models in R programming environment easily and quickly.

## Modeling Environment

Rgogo is developed in R programming language.  Modelers are required to use R as the modeling language in order to use Rgogo.

Rgogo is cross-platform.  You can install Rgogo in all operating systems that are supported by R including Windows, MacOS or Linux.  This makes actuarial models portable and shareable.  Any model that is built on one platform can also be modified or run on a different platform without code changes.

## Design Objectives

The design of Rgogo adopts object-oriented programming (“OOP”) principles.  OOP principles allow Rgogo to separate actuarial logic and computing algorithm.  Each Rgogo object represents a modeling component that an actuary is familiar with such as a product, a policy, an assumption or a table.  The complex calculation formulae implemented for the modeling component are encapsulated inside the class design and isolated from modelers.  This allows a modeler to focus on actuarial principles and business logic without being stuck in programming details when conducting a modeling exercise.

Rgogo is designed to achieve the following objectives:

* __Ease of Use__.  Rgogo is for modelers of varying levels of programming skill.  With basic knowledge in R, a modeler can use Rgogo to build and run a complex actuarial model easily and quickly.

* __Customizability__.  Rgogo provides flexibility in customizing model components to meet an individual modeler’s special needs when required by innovative ideas.

* __Transparency__.  Rgogo ensures model clarity and auditability by adopting open-source approach and promoting consistent coding styles.   It allows anyone to inspect the codes and validate the calculation.  

* __Scalability__.  Rgogo is designed to accommodate modeling projects of all sizes, ranging from stand-alone projects of solo modelers to large projects requiring team collaboration.  


## Areas of Application

Rgogo is designed for complex actuarial modeling involving life contingency.  The primary areas of application of such models are life insurance and pension.

Common types of project that can be done with Rgogo include:

* valuation

* pricing

* cashflow projection

* scenario testing

## Rgogo Project Website

Please visit [Rgogo project website](https://rmodel.io) for more information.
