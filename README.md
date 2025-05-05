<div align="center">

# Icicle
### The Icicle Streaming Query Language.

[![Build Status](https://github.com/icicle-lang/icicle/actions/workflows/haskell.yml/badge.svg)](https://github.com/icicle-lang/icicle/actions/workflows/haskell.yml)

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
options for creating features. Languages like R provide an extremely expressive
environment, but they don't scale well to the gigabyte, terabyte, or petabyte
level, and can't guarantee a lack of runtime issues or type errors.

Likewise, SQL can be applied for machine learning features, but is clunky to write
when we need to compose and extract hundreds of features, and it makes time
travelling queries (what did a customer look like at a particular past date),
and label leakage prevention extremely challenging.

Icicle is a total programming language designed to provide `O(n)` runtime for
all feature queries, while providing a pleasant environment for data
scientists and engineers to write expressive features.

Type System
-----------

Icicle achieves its goals by using a distinctive, modal type system which
guarantees that all queries must be computable within a single pass.


The simplest examples and counter-examples one may consider are `mean` and
`variance`. First up, one could write `mean` as:

```haskell
mean : Element Double -> Aggregate Double
mean v = sum v / count v
```

This is fine, and one can be sure that Icicle will fuse the sum and count
queries such that the data will only be visited once. For calculating the
variance and standard deviation, one might naïvely try this:

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
the sum of differences squared. In Icicle, this version of variance is a type
error, and we instead provide Welford's numerically stable streaming
calculation for variances.


Facts & Values
--------------

Icicle is designed for use with a Fact store – a database which store time
ordered events for an entity.

Facts are (richly typed) values, keyed along three dimensions:

 - The entity, this would be typically thought to represent the primary
   key of a row in a traditional data base, and could be something like
   a customer;

 - Attribute, this would be typically thought to represent the name of
   a column in a traditional data base. It's the kind of event that is
   being stored (a demographics change or a transaction for example);
   and

 - Time, this represents when a fact is valid. Different types of
   facts may interpret this in different ways (for example for a state
   like value, this would indicate a fact is valid from the time until
   the next fact with the same entity / attribute offers a more recent
   value. There is no analog in traditional data bases, but
   this is more common in immutable or append-only data stores.

Values themselves are strongle typed, and may be primitives, structs,
lists of values, or rust style enums (sum types).


Data Processing
---------------

Data processing for Icicle is heavily parallelized. This places
restrictions on how data is processed and how expressions can
relate to each other - in most cases these restrictions are
simplifying to the design of icicle.

The basic invariants are:

 - Data is processed in "batches", where a batch has a set of
   uniform properties:

 - All facts in a batch are for the same entity.

 - All facts in a batch are for the same attribute.

 - Facts in a batch are processed in chronological order.

 - A batch is _guaranteed_ to have _all_ facts for a given
   entity / attribute.

When processing data within a streaming engine (such as an
Icicle Kafka consumer), we can relax the last constraints by
storing incremental values, so that we can resume the computation
as more data arrives.


Expressions
-----------

Icicle supports a wide variety of expressions, and queries which
can be computed in an event sourcing or streaming manner should be
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

  [ambling]: https://github.com/ambiata/icicle/blob/master/doc/user/ambling.md
  [Icicle: The Highs and Lows of Optimising DSLs]: http://cufp.org/2016/the-highs-and-lows-of-optimising-dsls.html
