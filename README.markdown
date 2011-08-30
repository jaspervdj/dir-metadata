dir-metadata
============

What?
-----

`dir-metadata` is a tool which allows you to associate loose metadata with
certain directories.

All data is stored in a single file in your home directory. I consider this a
feature, because this ensures the actual directories stay clean. However, I
understand that having the metadata local to the directories might be useful as
well -- support might be added for this in the future.

Installation
------------

Grab it from [Hackage] (once I add it there):

    cabal install dir-metadata

[Hackage]: http://hackage.haskell.org/

Example usage
-------------

I have `dm` aliased to `dir-metadata` to save some typing:

    alias dm=dir-metadata

I use this tool when I'm watching a set of videos. I usually have a hard time
remembering which episode I last saw, so this is where `dir-metadata` comes in
handy.

    videos$ ls
    Episode 01.avi
    Episode 02.avi
    Episode 03.avi
    ...

When I start watching episode 1, I use:

    videos$ dm add Watching epi 1

Next time I'm the mood for some entertainment, I can query the metadata:

    videos$ dm
    1. Watching epi 1
    videos$ dm add Watching epi 2
    videos$ dm
    1. Watching epi 1
    2. Watching epi 2

In my home folder, I keep a short TODO-list.
    
    ~$ dm
    1. Write a decent readme for dir-metadata
    2. Actually get some work done
    3. Watch another episode!

You can remove metadata by using the `rm` command. For example, let's suppose
we're done with (1) and (2):

    ~$ dm rm 1 2
    ~$ dm
    1. Watch another episode!

Reference
---------

TODO
