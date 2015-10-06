# mighty-metropolis [![Build Status](https://secure.travis-ci.org/jtobin/mighty-metropolis.png)](http://travis-ci.org/jtobin/mighty-metropolis)

The classic Metropolis algorithm.  Wander around parameter space according to a
simple spherical Gaussian distribution.

Exports a `mcmc` function that prints a trace to stdout, as well as a
`metropolis` transition operator that can be used more generally.

See the *test* folder for example usage.

    import Numeric.MCMC.Metropolis

    rosenbrock :: [Double] -> Double
    rosenbrock [x0, x1] = negate (5  *(x1 - x0 ^ 2) ^ 2 + 0.05 * (1 - x0) ^ 2)

    main :: IO ()
    main = withSystemRandom . asGenIO $ mcmc 10000 1 [0, 0] rosenbrock

![trace](https://dl.dropboxusercontent.com/spa/u0s6617yxinm2ca/osecfv_w.png)
