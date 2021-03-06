Frank Stella’s Summary
================
Phillip Harmadi

In this folder, Phillip will include his work on Frank Stella’s art
piece [***Lettre Sur Les Et Muets
II***](https://nsuartmuseum.org/exhibition/frank-stella-experiment-and-change/).
Born in 1936, Frank Stella is an American painter noted for his works in
the areas of minimalism and post-painterly abstraction genres.

Painted in 1974, *Lettre Sur Les Et Muets II* (1974) was one of his
synthetic polymer paint on canvas and is currently housed in the NSU Art
Museum.

<img src="stella.jpg" style="width:40.0%" />

The painting’s symmetrical and easily identifiable features are what
attracts us to recreate a visualization of this masterpiece. 4 distinct
features spark our interest. We also believe that these features should
be made *adjustable/modifiable* to the preference of each audience:

1.  **Number of layers**: In the original painting, there are 12 layers
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

2.  **Primary color sequence**: The original painting uses a
    rainbow-gradient color sequence as part of its 12 layers of
    colored-gradient rectangles. For the recreated visualization, the
    audience will be able to change this through adjusting the color
    palette and decide whether or not to reverse the direction of the
    color sequence.

3.  **Secondary color sequence**: The original painting uses a
    black-and-white-gradient color sequence as part of its 11 layers of
    non-colored-gradient rectangles. For the recreated visualization,
    the audience will be able to change this through adjusting the color
    palette and decide whether or not to reverse the direction of the
    color sequence.

4.  **Borderline**: We’ve noticed that the original artwork contains a
    thin white borderlines between each of the rectangle layers. We
    believe that some of our audience like this feature but not all,
    hence, we make it an option for the audience to preserve or remove
    the borderlines in their final plot.

-   ***Approach:*** We will use the ‘switch input’ Shiny widget in the
    UI interface to turn on/off the borderline.
    `switchInput(inputId = "borderline", label = "Borderlines", value = FALSE)`
    In the server, the reactive input ‘borderline’ will be part of the
    aesthetic in the ggplot. `geom_polygon(..., size = borderline())`
