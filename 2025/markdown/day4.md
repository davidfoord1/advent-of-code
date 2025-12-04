A happy little grid puzzle :)

My initial approach was quite slow to run, so for part 2 I tried to cut down the search space by only checking rolls that had neighbours removed. After a bit of a convoluted refactor of my R part 2 solution I finally down to <0.2s which is good enough for me.

One improvement was implementing a clunky queue in a matrix because I like seeing array indices, using start and end pointers. Another big improvement was going from searching in 8 directions from each position to find roll counts, to essentially shifting the whole grid 8 times and using vectorised addition.

I then slapped together a fully brute force Python solution and it runs in <1s, *sigh*. Still, I was happy with my original time-to-solution.
