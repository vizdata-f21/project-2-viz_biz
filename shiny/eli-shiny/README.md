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

1.  **Text**: In the original piece, there are 3 rows of text (top, middle, and bottom). 
    We would like the audience to be able to determine what the text says,
    the size of the text, and the color of the text in their visualization.

-   ***Approach:*** We will use the ‘slider input’ Shiny widget in the
    UI interface to control the size of the text, three 'text input' Shiny widgets
    to control what is displayed in the three rows of text, and a 'color input'
    Shiny widget to control the color of the text. All three types of inputs will be 
    used to set visual properties in the ggplot visualization. 

2.  **Border/Labels**: The original piece contains a border around the print as 
    well as colored bars behind the text (that make the text standout). 
    For the recreated visualization, the audience will be able to change the 
    color and size of the border, as well as the color of the label.
    
-   ***Approach:*** We will use the ‘slider input’ Shiny widget in the
    UI interface to control the size of the border and a 'color input' Shiny 
    widget to control the color of the border and background labels. The input for 
    border color and label color will be the same. Both types of inputs will be 
    used to set visual properties in the ggplot visualization. The reactive dimension
    variables (which will get created from the reactive link/url variable) will be 
    integral components in specifying the location of the border around the image 
    as well as the location of the labels (and text) on the image.

3.  **Background Image/Image Link**: The original piece uses a
    black-and-white portrait as a backdrop. For the recreated visualization,
    the audience will be able to change this image by pasting an image address
    from the web into a text input. The visualization will then dynamically change
    to have the new image be the backdrop, with all elements being scaled to fit 
    the dimensions of the updated image. The audience will also beable  to adjust the brightness,         saturation and hue of the image using 'slider input' Shiny widgets.

-   ***Approach:*** We will use a ‘text input’ Shiny widget in the
    UI interface to access the user-inputted link. In the server, the reactive 
    input ‘path’ (which is the variable name for the link) will be used to create
    variables for image dimensions (as mentioned above). Additionally, the link input
    will be read and used to display the background image.
