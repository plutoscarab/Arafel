# Arafel

A pet project to make a compiler for a functional programming language. I like
ML-style languages, and the compiler for this one is written in F#. The ultimate
goal would to get this language to a point where I could self-host it, i.e.
rewrite the compiler in Arafel instead of F#.

I called it Arafel because I wanted to use the ".af" file extension, and because
it's probably going to be slow as f*@# at first. REPL interpreter to start, then
full-file interpreter, then transpiler. I don't have much interest going further
than that.

I have a love/hate relationship with "partial application" in functional
programming. It's cool, it makes programs concise, but I find that it can make
code hard to read, especially when overdone, e.g. point-free contortionist
programming. But if you have lamda expressions there's no real way to disallow
partial application, e.g.

    let mag(x, y) = x * x + y * y

And then you can just do the currying yourself:

    let mag′(x) =
        (y) = mag′(x, y)

If I could outlaw point-free programming (it's for mathematicians, not for
readible code), I would, but we have to have lambdas, so I surrender.
