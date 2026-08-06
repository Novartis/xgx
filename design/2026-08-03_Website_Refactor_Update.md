## xGx Refactor

For many reasons, we'd like to update the xGx website.  The key guiding principle for the update though is that nothing should break.  Every page should continue to compile.  

This should all be doen in a new branch.

Before anything begins, look through the xGx MS Teams space and todos and issues and see what else should be added.

## Organization

- Should this site be organized a bit better?  I think so.  I don't like how so many files and folders are in the top directory.  Can this be reorganized without breaking anything.  
- Should the html files be committed in git or compiled in git and git-pages?
- The next version should be developde to enable AI development/use (i.e. use the pages as templates) but also there will be skill files to help with data checking and plot interpretation.

## xGx usage

- use a new caption that's a bit simpler.
- change the way xgx_scale_x_time_units is used.  Scaling time in the ggplot() object and just using xgx_scale_x_time_units to set breaks and ticks.  Ah, maybe even create a new function for this, to put in xgxr.  like xgx_breaks_x_time_units() or something like that
- look for places where code might be improved - like the dosing in the tumor size RECIST plots

## Added functionality

- The data checking and data exploration.  I had generated ideas for additional plots, maybe from dose finding toolbox.  Improve that.  Also, look for ideas for how to improve.  Think about if synpmx sohuld relate in some ways here as there is a validate function.  I've purposefully gone away from functions because they do make it harder for people to run and understand code line by line...  Revisit that idea