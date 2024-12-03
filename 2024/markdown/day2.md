Part 1 was straightforward. For part 2 I took what seems to be a long-winded
approach of trying to identify which levels caused unsafe differences. Then I
tried reports with those levels removed and tested if they were safe. This
included checking for a majority direction of differences and replacing the
differences in the minority, but on my first attempt I forgot to deal with no
majority!

I've seen from others that it's simpler to try removing each and every level,
which is plenty feasible with the input. I preferred to use this simpler logic
in my Python implementation as working with its lists and numpy arrays are new
to me.
