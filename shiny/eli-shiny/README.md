eli-shiny
================
Eli Feder

In this folder, Eli will include his work on Barbara Kruger’s art
piece [***Untitled (Your body is a battleground)***](https://www.thebroad.org/art/barbara-kruger/untitled-your-body-battleground).
Born in 1945, Barbara Kruger is an American conceptual artist and collagist known for her unique style that consists of black-and-white photographs, overlaid with declarative captions, stated in white-on-red bold text.

*Untitled (Your body is a battleground)* (1989) was produced by Kruger for the Women’s March on Washington in support of reproductive freedom. It is currently housed in the Broad Museum in Los Angeles, Californiaa.

<img src="kruger.jpg" style="width:40.0%" />

The piece’s easily simple,yet eye-catching features are what attracts us to 
recreate a it as a visualization. 4 distinct features spark our interest.
We also believe that these features should be made modifiable to the preference 
of the user:

1.  **Text**: In the original painting, there are 12 layers
    of colored-gradient rectangles and 11 layers of
    black-and-white-gradient rectangles (23 layers in total),
    alternating against one another. We would like the audience to be
    able to determine how many layers do they want to have in total in
    their final piece of art, depending on the purpose of usage
    (small/nightstand or big/wall decorations).

-   ***Approach:*** We will use the ‘slider input’ Shiny widget in the
    UI interface.
    `sliderInput(inputId = "size", label = "Size:", min = 10, max = 50, value = 20)`
    In the server, the input ‘size’ will and integral component to
    create the dataframe that will be used in the plotting process.
    `n <- reactive({input$size * 2})`

2.  **Border/Labels**: The original painting uses a
    rainbow-gradient color sequence as part of its 12 layers of
    colored-gradient rectangles. For the recreated visualization, the
    audience will be able to change this through adjusting the color
    palette and decide whether or not to reverse the direction of the
    color sequence.
    
-   ***Approach:*** We will use the ‘slider input’ Shiny widget in the
    UI interface.
    `sliderInput(inputId = "size", label = "Size:", min = 10, max = 50, value = 20)`
    In the server, the input ‘size’ will and integral component to
    create the dataframe that will be used in the plotting process.
    `n <- reactive({input$size * 2})`    

3.  **Background Image/Image Link**: The original painting uses a
    black-and-white-gradient color sequence as part of its 11 layers of
    non-colored-gradient rectangles. For the recreated visualization,
    the audience will be able to change this through adjusting the color
    palette and decide whether or not to reverse the direction of the
    color sequence.

-   ***Approach:*** We will use the ‘switch input’ Shiny widget in the
    UI interface to turn on/off the borderline.
    `switchInput(inputId = "borderline", label = "Borderlines", value = FALSE)`
    In the server, the reactive input ‘borderline’ will be part of the
    aesthetic in the ggplot. `geom_polygon(..., size = borderline())`
