# TN_COVID

R scripts for graphing COVID-19 data as released by the TN Dept of Health

# What you will need to play with the code:

* A Windows, Linux, or Mac computer.    Linux or Mac are preferred.   R on Windows can't antialias graphics properly, so the infographics look pixelated, but otherwise still works.

* R 3.6.x.   It probably works on older version too.  It's possible that R 4.0 or greater will also work, I haven't tested it yet because 4.0 breaks a few packages I use elsewhere.   You can download it at:  https://cran.r-project.org/

* The latest RStudio for your platform.   It works fine on both RStudio Desktop and RStudio Server (I use the latter).   Unless you know better, you probably want the Desktop version.  Download them at:   https://rstudio.com/products/rstudio/download/

The script has a bit of code at the top to install any necessary packages that aren't currently installed the first time you run it.

If you have the time and inclination (or are a masochist)...   The BLAS (a library used for matrix computations) included with R is tragically slow.   If you're compiling R from scratch, or can find pre-built DLL's for your platform, I highly recommend using GotoBLAS, OpenBLAS, ATLAS, or Intel MKL instead.   If you're running R a zillion times like I do, it will save you time.  If you don't know what those are, just ignore it.   Clear Linux is probably the fastest platform for running R, but getting it working is left as an excercise for the reader...

R is also tragically single-threaded (you *can* use threads, but it's painful compared to other languages), so don't worry about trying to throw an expensive 16 core CPU at it for speedups.    Your bog-standard Haswell 4-core is sufficient. 

# How do I run it?

Start up RStudio, load TN_COVID.R, and hit the "source" button.  

# What can you do to help?   Ideas...

The code *works*, but it started off doing a small limited graph or two after I'd been studying R for only a few months, and has morphed into something much larger, and has had many adventures along the way.  While it has been a very educational experience, to put it gently, it's not beautiful code, and can definitely be improved upon.

* (Easy) Do some code janitoring.   Run R's lint() on files and clean up things (just remember that lint gives *suggestions*, not always good advice).   Add comments to the code.  Fix any stupid mistakes I made.

* (Easy) The graphs/maps that *aren't* being used in the infographic have bitrotted a bit.   Give them a little TLC to get them working again.   Double-check the output against the state's numbers to make sure it's correct.   I can fix one up in about 5-10 minutes.  Thus far it's been about putting na.omit() in the proper spot.

* (Easy) Make a new graph or map.   Again, if the state has similar data, compare against it to make sure it's working correctly.

* (Medium) Do a code clean-up.   I'm sure my code is engaging in some unnecessary contortions, either because it's "evolved", or because I don't know what I'm doing. 

* (Harder) Modify it to graph data for another state, or perhaps a version using the Nashville Health Dept data.

* (Harder) Make a Shiny server so people can assemble an infographic with the stats they want to see. 

# Example render using defaults:

![Example render from July 21](https://i.imgur.com/mbukJTK.png)
