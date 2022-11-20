# Arafel

A pet project to make a compiler for a functional programming language. I like
ML-style languages, and the compiler for this one is written in F#. The ultimate
goal would to get this language to a point where I could self-host it, i.e.
rewrite the compiler in Arafel instead of F#.

I called it Arafel because I wanted to use the ".af" file extension, and because
it's probably going to be slow as f*@# at first. I don't have much interest
going further than doing a transpiler. I hate REPLs but I started one anyway,
but now I'm abandoning it. I'm a compiler-loop guy.

I have a love/hate relationship with "partial application" in functional
programming. It's cool, it makes programs concise, but I find that it can make
code hard to read, especially when overdone, e.g. point-free contortionist
programming. But if you have lamda expressions there's no real way to disallow
partial application, e.g.

    let mag(x, y) = x * x + y * y

And then you can just do the currying yourself:

    let mag′(x) =
        (y) = mag(x, y)

If I could disallow point-free programming in Arafel (it's for mathematicians,
not for readible code), I would, but we have to have lambdas, so I surrender.
For point-free code lovers, I give you Haskell's

    mag = (. join (*)) . (+) . join (*)
