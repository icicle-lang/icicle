<div align="center">

# Icicle
### The Icicle Streaming Query Language.

[![Build Status](https://api.travis-ci.com/icicle-lang/icicle.svg?branch=master)](https://travis-ci.com/icicle-lang/icicle)

</div>

Purpose
-------

Icicle is a language designed for easy collaboration in feature
engineering and business intelligence without fear or difficulty.

Using static type checking with a novel, modal type system, we can
be 10x faster than Spark, allow different users' queries to be
efficiently fused, and guarantee that if a query type checks, it
won't crash or interfere with any other.

Icicle is a simple language designed for data scientists, data
engineers, and business intelligence professional to achieve state
of the art performance without difficulty.


The key principles of Icicle are to:

 - Permit different users to build queries independently, but execute
   them together efficiently with sharing and non-interference;
 - Provide a static guarantee that all computations must be computed in
   a single pass;
 - Use a first class notion of time - one should be able to query any
   entity's state and features at any time (this is important for
   preventing label leakage for instance); and
 - Use query fusion and high level optimisations to achieve great
   performance.


Motivation
----------

When performing a data engineering and machine learning tasks, one has many
options for creating features. Languages like R can provide expressivity,
but they don't scale well to the gigabyte, terabyte, or petabyte level; SQL
can be applied for machine learning features, but is clunky to write, can
fail at runtime, its hard to protect against label leakage, and its runtime
order is hard to quantify, especially at the terabyte and petabyte levels.

Icicle is a total programming language designed to provide O(n) runtime for
all feature queries, while providing a pleasant environment for data
scientists and engineers to write expressive features.

Examples
--------

The simplest examples and counter-examples one may consider are `mean` and
`variance`. First up, one could write `mean` as:

```haskell
mean : Element Double -> Aggregate Double
mean v = sum v / count v
```

This is fine<sup>[1](#stability)</sup>, and one can be sure that Icicle will
fuse the sum and count queries such that the data will only be visited once.
For calculating the variance and standard deviation, one might naïvely try
this:

```haskell
variance : Element Double -> Aggregate Double
variance v =
  let
    mean'  = mean v              -- Aggregate Double
    count' = count v             -- Aggregate Double
    sq2    = sum ((v - mean')^2) -- Illegal subtraction of Aggregate from Element
  in
    sq2 / count'
```

But clearly, this has a massive problem. The data must be traversed twice
to calculate this query: first to calculate the mean, and then to calculate
the sum of squares differences. In Icicle, this version of variance is a type
error, and we instead provide Welford's numerically stable streaming
calculation for variances.

Context
-------

Icicle is designed for, but not dependent on, the
[ivory](https://speakerdeck.com/ambiata/ivory-concepts)
data-store. While parts of this document uses the terms of ivory,
the problems being addressed are not unique to ivory, and one can adapt
these ideas to different contexts. For an idea of what ivory does, see

 - [https://speakerdeck.com/ambiata/ivory-concepts](https://speakerdeck.com/ambiata/ivory-concepts)

 - [https://speakerdeck.com/ambiata/ivory-a-data-store-for-data-science](https://speakerdeck.com/ambiata/ivory-a-data-store-for-data-science)

 - [https://speakerdeck.com/ambiata/ivory-data-modelling](https://speakerdeck.com/ambiata/ivory-data-modelling)

 - [https://github.com/ambiata/ivory](https://github.com/ambiata/ivory) *(internal Ambiata only)*


Facts & Values
--------------

Facts are (typed) values, keyed along three dimensions:

 - Entity, this would be typically thought to represent the primary key of
   a row in a traditional data base.

 - Attribute, this would be typically thought to represent the name of
   a column in a traditional data base.

 - Time, this represents when a fact is valid at. Different types of
   facts may interpret this in different ways (for example for a state
   like value, this would indicate a fact is valid from time (t) until
   the next fact with the same entity / attribute and a more resent
   time dimension. There is no analog in traditional data bases, but
   this is more common in immutable or append-only data stores.

Values themselves are structured, and may be primitives, structs,
or lists of values.

Data Processing
---------------

Data processing in Ivory (and similar data stores) is heavily
parallelized. This places restrictions on how data is processed
and how expressions can relate to each other - in most cases
these restrictions are simplifying to the desigin of icicle.

The basic invariants are:

 - Data is processed in "batches", where a batch has a set of
   uniform properties:

 - All facts in a batch are for the same entity.

 - All facts in a batch are for the same attribute.

 - Facts in a batch are processed in chronological order.

 - A batch is _guaranteed_ to have _all_ facts for a given
   entity / attribute.


Expressions
-----------

Icicle supports a wide variety of expressions, and queries which
can be computed in an event soucing or streaming manner should be
computable in Icicle.

The best place to get a feel for expressions is the [ambling]
document, which gives a run through of some queries, and how Icicle
is different to other query languages.

Optimisation
------------

Icicle has a highly optimising backend, which compiles queries to
C programs operating on flattened data structures. A great introduction
to Icicle's optimisations is a talk by one of its authors, Jacob Stanley:
[Icicle: The Highs and Lows of Optimising DSLs].

<a name="stability">1</a>: Actually, this isn't numerically stable, in the
  icicle prelude, we use a more robust version.

  [ambling]: https://github.com/ambiata/icicle/blob/master/doc/user/ambling.md
  [Icicle: The Highs and Lows of Optimising DSLs]: https://www.youtube.com/watch?v=ZuCRgghVR1Q
