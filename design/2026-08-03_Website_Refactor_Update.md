## xGx Refactor

For many reasons, we'd like to update the xGx website.  The key guiding principle for the update though is that nothing should break.  Every page should continue to compile.  

This should all be doen in a new branch.

Before anything begins, look through the xGx MS Teams space and todos and issues and see what else should be added.

## Organization

- Should this site be organized a bit better?  
- Should the html files be committed in git or compiled in git and git-pages?

## xGx usage

- use a new caption that's a bit simpler.
- change the way xgx_scale_x_time_units is used.  Scaling time in the ggplot() object and just using xgx_scale_x_time_units to set breaks and ticks.  Ah, maybe even create a new function for this, to put in xgxr.  like xgx_breaks_x_time_units() or something like that
- look for places where code might be improved - like the dosing in the tumor size RECIST plots

## Added functionality

- The data checking and data exploration.  I had generated ideas for additional plots, maybe from dose finding toolbox.  Improve that.  Also, look for ideas for how to improve.  Think about if synpmx sohuld relate in some ways here as there is a validate function.  I've purposefully gone away from functions because they do make it harder for people to run and understand code line by line...  Revisit that idea

## Add pages or significant updates

- Significant update to Data checking.  Look at the IDA page from dose finding toolbox.  Ask for ideas for what else to add.
- Causality Page
- Logistic regression pages?  I might want to add some other things from what I was doing from dose finding toolbox
- MMRM?

## Scope Expansion/Creep

- There are useful things to have, like thinking about logistic regression and E-R that are a bit out of scope but this is a natrual place to put things.  Thinks like choosing an exposure metric, how to assess a logistic regression, etc.  Think about where and how to put this stuff here.  It's beyond exploratory graphisc, but this is a natural place to put useful material.
