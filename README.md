# hu.dwim.rdbms

## What

It's a Common Lisp library to connect to rdbms (database) systems.

Backends:
 - PostgreSQL (battle tested)
 - Sqlite
 - Oracle

## Where

At the project's [github page](https://github.com/hu-dwim/hu.dwim.rdbms).

## Status

This codebase has been mostly abandoned since around 2011. It was
pretty stable with the PostgreSQL backend when we last worked on it.

We have one active project in 2026, which may or may not result in
some new commits holding back the bitrot.

An Oracle backend was developed independently by Davit Lichteblau and
Tomas Hlavaty. A lot of it has been merged into `main`, but some of it
remains in the `oracle` branch. [Kambiz
Darabi](https://github.com/darabi) made some additional changes to it,
which is kept in the `oracle-darabi` branch. You may want to look at
[his fork](https://github.com/darabi/hu.dwim.rdbms), too.

The oracle effort and its merging was once documented on [this
url](http://src.knowledgetools.de/tomas/porting/), but it's long gone
by now.
