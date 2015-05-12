## Tutorial: Extacting land cover information for a Neighborhood Nestwatch study region

1. Get raw impervious surface data

    a. Go to the National Map Viewer website, located at: http://viewer.nationalmap.gov/viewer/
    
    b. Zoom in to your study region. Make sure you can see your **entire** region on the map
    
    c. Select the *Advanced* tab.
    
    d. Select the *Range Ring* tool (far right button).
    
    e. Place your mouse pointer at the center of your study region and select. In a new window you will prompted to enter the number of rings and buffer distance. Select one ring and a buffer distance of 100 km. Then *Draw rings*.
    
    g. With your mouse pointer, select the ring. In the new window that appears, select *Download*.
    
    h. Under the _Data_ subheading, select "_Land cover_". Scroll to the bottom of the window and select _Next_.
    
    i. Select the file "_National Land Cover Database 2006 (2011 edition) - Impervious Surface Percentage - 3x3 Degree Extents_". This will add that layer to your _Cart_.
    
    j. Click the _Checkout_ button. You will then be prompted to enter your email address (twice). Do so and click _Place Order_. Download links for the file or files will then be sent to your email address.
    
    k. Go to your email. In the message from National Map Viewer, you will see multiple data links. Download those links to a folder that can be easily accessed.
    
2. Open R Studio. If you do not yet have R Studio, it can be downloaded at http://www.rstudio.com/products/rstudio/download/. Additionally, R Studio requires that you have R on your computer (version 2.11.1 or higher). If you do not yet have R, it can be downloaded at http://cran.rstudio.com/. Both R and R Studio are free and open source software.

3. Go to the Neighborhood Nestwatch page of my GitHub website at https://github.com/bsevansunc/Nestwatch
    
