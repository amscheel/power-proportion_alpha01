# power-proportion_alpha01

I wanted to know how many 'significant' p-values (for alpha = .05, i.e. p < .05) can be expected to be below .01 for different levels of power.
This repo has the R code to create plots I showed in [a Twitter thread](https://twitter.com/annemscheel/status/1307675578271764483?s=20) I wrote about it.

Here's the text of the thread, in case it gets lost:

> Learning how p is distributed when H0 is false* made me wary of p-values between .01 and .05, which easily morphs into 'if it's not <.01 it's probably not real'.
> I didn't trust my intuition about that heuristic, so I made some plots 1/
>
> *h/t @krstoffr https://rpsychologist.com/d3/pdist/

> Of course the probability that a true non-zero effect will give you p < .01 is simply the power for alpha = .01. So let's first compare power for alpha = .01 with power for alpha = .05:
> 2/

> The plots look similar at first glance  but it makes quite a difference in practice.
> E.g., in a two-sample t-test to detect a true effect of d = .3 with 80% power, you need a total sample of N = 352 for alpha = .05 and N = 524 for alpha = .01 (almost 50% more). 
> 3/

> What I wanted to know: when people use alpha = .05, what proportion of significant p-values should be below .01 (when there is a true effect)?
> When power is low (small d and/or N), it can easily be less than half (it never drops below 20% because that would be a flat p-curve!)
> 4/

> I first plotted this with power for alpha = .05 on the x-axis because it combines the information for N and d and because we arguable have an intuition for 'that' power. It got unexpectedly confusing though so I'm a little less sure what to make of that plot.
> 5/

> But this last plot makes one thing clear: If you want a large proportion of significant (p<.05) p-values to be below .01 - say, 90% - you need REALLY good power for alpha=.05! That shouldn't be surprising since it's just maths, but sadly my brain doesn't have G*Power installed
> 6/

> Based on this little ggplot adventure, I'll try to update my intuition: 
> 1) when power isn't abysmal, a majority of significant p-values should be <.01 
> 2) 'a majority' does NOT mean 'virtually all' 
> 3) point 2) is really important 
>
> The end! 7/7

> P.S. When we say an effect 'probably isn't real' we'll inevitably end up discussing SESOIs. I think it's plausible that most/many psych experiments have some residual bias/demand effects and most d's around .1 are probably not the effect experimenters were hoping to be 'real'.
