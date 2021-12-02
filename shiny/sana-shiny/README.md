sana-shiny
================

In this folder, Sana will include her work on the Piet Mondrian art piece (https://www.wikiart.org/en/piet-mondrian/trafalgar-square-1943).

Born in 1872, Piet Mondrian was a Dutch artist considered one of the pioneers
of 20th century abstract art. 

Painted from 1939-1943, *Trafalgar Square* was the first in a series of paintings
Mondrian created named after locations in cities that gave him refuge during World War II. 

<img src="mondrian.jpg" style="width:30.0%" />

The painting’s geometric shapes and lines are what allowed us to recreate this 
visualization in ggplot2. 2 distinct
features sparked our interest to make *adjustable/modifiable* to the preference 
of each audience:

1.  **Horizontal Lines**: In the original painting, there are 4 horizontal lines
that seem to be evenly distributed within the middle of the painting. We would like the 
audience to be able to be able to choose how many horizontal lines are in the piece. 
This can help them adjust the abstract feel of the art and 
to make it more to their liking. 

-   ***Approach:*** We will use the ‘slider input’ Shiny widget in the
    UI interface.
    `sliderInput("piet_lines",
            "Number of Full Horizontal Lines:",
            min = 0, max = 5, value = 4, ticks = FALSE
          )`
    Because the original four horizontal lines are defined by one specific function (geom_hline()), 
    we were able to pass the input from the UI sliderinput to a parameter in the geom_hline()
    to change the number of horizontal lines between the defined upper and lower bounds. 
    

2.  **Colors**: The original painting uses four distinct colors: blue, black 
red and yellow in the different colored boxes. It also uses one specific color for
the lines: black. We wanted to give users the ability to change 
the colors of the rectangles, the gridlines, and the background color
based on their likings and to understand how a new color scheme 
changes the vision of the art. 


-   ***Approach:*** To do this, we will use the 'colourInput' in the UI interface
to allow users to select four colors out of a full color spectrum. 