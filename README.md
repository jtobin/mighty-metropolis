# mighty-metropolis

The classic Metropolis algorithm.  Wander around parameter space according to a
simple spherical Gaussian distribution.

Exports a `mcmc` function that prints a trace to stdout, a `chain` function for
collecting results in memory, and a `metropolis` transition operator that can
be used more generally.

See the *test* directory for example usage.

    import Numeric.MCMC.Metropolis

    rosenbrock :: [Double] -> Double
    rosenbrock [x0, x1] = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)

    main :: IO ()
    main = withSystemRandom . asGenIO $ mcmc 10000 1 [0, 0] rosenbrock

*mighty-metropolis* is a member of the [declarative][decl] suite of libraries,
containing a bunch of MCMC algorithms that play nicely together.

[decl]: https://github.com/jtobin/declarative
