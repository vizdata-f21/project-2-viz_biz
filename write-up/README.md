# Project 2 Write-up (Viz Biz)

*Author: Eli Feder, Lillian Clark, Phillip Harmadi, Sana Pashankar*

## Introduction

For this project, we created a convenient website where non-R users and the general public can navigate to and customize several famous modern art pieces recreated in ggplot, modifying some of the artwork aesthetics to make the masterworks their own. Our goal specifically was to promote digital extensions of modern, abstract paintings and to allow users to make art personalized to their taste, which they can download for their own use. Furthermore, in this project, we strived to increase public appreciation and awareness of using digital techniques, such as ggplot, to recreate art or make original digital art. 

We plan to recreate and make customizable the following four images:

#### Frank Stella, *Lettre sur les sourds et muets II* (1974) [1]

#### Barbara Kruger, *Untitled (Your body is a battleground)* (1989) [2]

#### Wassily Kandinsky, *Composition 8* (1923) [3]

#### Piet Mondrian, *Trafalgar Square* (1939-1943) [4]

To recreate these modern art pieces, we will be creating our own dataframes, which will incorporate user input to customize certain artistic aspects of each piece. We will also utilize mathematical functions and specific packages such as `library(colourpicker)` and `library(generativeart)` to facilitate the digitization of these art pieces. 

We will accumulate these recreated, customizable art pieces in a Shiny App. Within this Shiny App,
there will be an initial tab with a description of how to use the app to customize and download 
these art pieces. Then, there will be a separate tab for each of the pieces. We will allow users 
to change features such as colors, number and amount of certain shapes, transparency, etc
throughout the pieces. 


## Approaches 

### Frank Stella, *Lettre sur les sourds et muets II* (1974)

<img src="images/stella.jpg" style="width:24%"/>

The painting’s symmetrical and easily identifiable features are what
attracts us to recreate a visualization of this masterpiece. 4 distinct
features spark our interest. We also believe that these features should
be made *adjustable/modifiable* to the preference of each audience:

**Number of layers**

In the original painting, there are 12 layers
of colored-gradient rectangles and 11 layers of
black-and-white-gradient rectangles (23 layers in total),
alternating against one another. We would like the audience to be
able to determine how many layers do they want to have in total in
their final piece of art, depending on the purpose of usage 
(small/nightstand or big/wall decorations).

To alter the number of alyers, we will use the ‘slider input’ Shiny widget in the
UI interface.`sliderInput(inputId = "size", label = "Size:", min = 10, max = 50, value = 20)`
In the server, the input ‘size’ will and integral component to
create the dataframe that will be used in the plotting process.
`n <- reactive({input$size * 2})`

**Primary color sequence**

The original painting uses a rainbow-gradient color sequence as part of its 
12 layers of colored-gradient rectangles. For the recreated visualization, the
audience will be able to change this through adjusting the color
palette and decide whether or not to reverse the direction of the color sequence.

**Secondary color sequence**

The original painting uses a black-and-white-gradient color sequence as part 
of its 11 layers of non-colored-gradient rectangles. For the recreated visualization,
the audience will be able to change this through adjusting the color palette and
decide whether or not to reverse the direction of the color sequence.

**Borderline**

We’ve noticed that the original artwork contains a thin white borderlines 
between each of the rectangle layers. We believe that some of our audience 
like this feature but not all, hence, we make it an option for the audience to 
preserve or remove the borderlines in their final plot.

To do this, we will use the ‘switch input’ Shiny widget in the UI interface to 
turn on/off the borderline.`switchInput(inputId = "borderline", label = "Borderlines", value = FALSE)`
In the server, the reactive input ‘borderline’ will be part of the aesthetic 
in the ggplot. `geom_polygon(..., size = borderline())`

### Barbara Kruger, *Untitled (Your body is a battleground)* (1989)

<img src="images/kruger.jpeg" style="width:18%"/>

### Wassily Kandinsky, *Composition 8* (1923)

<img src="images/kandinsky.jpeg" style="width:28%"/>

### Piet Mondrian, *Trafalgar Square* (1939-1943) 

<img src="images/mondrian.jpg" style="width:15%"/>

This painting’s geometric shapes and lines are what allowed us to recreate this 
visualization in ggplot2. 2 distinct
features sparked our interest to make *adjustable/modifiable* to the preference 
of each audience:

**Horizontal Lines**

In the original painting, there are 4 horizontal lines that seem to be evenly 
distributed within the middle of the painting. We would like the  audience to be 
able to create more horizontal lines between the upper and lower bounds of the 
original horizontal lines to adjust the abstract feel of the art and  to make it 
more to their liking. 

Specifically to adjust these lines, we will use the ‘slider input’ Shiny widget in the UI interface
`sliderInput(inputId = "lines", label = "Number of lines:", min = 0, max = 5, value = 4)`
Because the original four horizontal lines are defined by one specific function `(geom_hline())`, 
we were able to pass the input from the UI `sliderInput` to a parameter in the `geom_hline()`
to change the number of horizontal lines between the defined upper and lower bounds. 
    
**Colors**

The original painting uses four distinct colors: blue, black 
red and yellow in different colored boxes. We wanted to give users the ability to change 
these colors based on their likings and to understand how a new color scheme 
changes the vision of the art. 

To do this, we will use the `checkboxGroupInput` in the UI interface
to allow users to select four colors out of list of multiple color options. If the user picks 
more than four colors, only the first four colors will show up. If the user uses less than 
four colors, the boxes corresponding to the missing color will show up blank (i.e. white). 

## Discussion

Within this project, one challenge all of us faced was learning the nuances of Shiny Apps. Before this project, all of us had little to no experience with Shiny App, and the grammar of the platform and creating the app was an unfamiliar process we had to learn.

We learned a lot about the behavior of Shiny reactive inputs and how we are required to use
`reactive({...})` whenever we are trying to define a variable that is dependent on reactive
input(s). One of the biggest challenge we have tackled is to create a non-static dataset when
recreating Frank Stella's artwork. As a user modifies the number of layers in the artwork, the
dataset will adjust automatically and dynamically, hence our team created a function with the
number of layers (the reactive input) as one of the parameters of the function.

Another challenge that we encountered was allowing for a user to input an image link of their choice in the Barbara Kruger section of the Shiny app. Not only was the link input reactive, but we encountered an error where the app would crash if an invalid url was used. To address this, we kept the original link variable if the link was valid and updated it to the kruger image link if not. We also added an error message if the link was not valid. 

