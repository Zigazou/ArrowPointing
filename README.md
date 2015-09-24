ArrowPointing
=============

This small project tries to follow an arrow and output the character it is
pointing to. It follows the code golfing of described at 
http://codegolf.stackexchange.com/questions/57952/where-is-the-arrow-pointing
except it does not golf :-)

Rules
-----

Here are the rules from the challenge:
> The input will be a multiline-string in which you need to find the character
> the arrow is pointing. There will only be one valid arrow. The valid arrow
> will only point to alphanumeric characters excluding S. A line will never
> overlap itself. e.g. `-|-`
>
>  - `S` (capital) represents where the arrow starts.
>  - `-` represents a horizontal line
>  - `+` represents a possible change in axis. A valid arrow will never start
>     with a `+`. Another `+` may be adjacent but the arrow should prioritize
>     going on a `-` or `|` first.
>  - `|` represents a vertical line
>  - `> < V ^` any of these represent the arrow head. These will never connect to a `+`.
>
> There will only be one `S` in the string. The input will also be padded to be a
> rectangle (not necessarily a square)

Additions
---------

The program is able to enhance the looking of each problem, like:

      +--S--+  
    O<|  |  |>O
      +-+++-+  
      +-+++-+  
      +-+++-+  
    O<|  |  |>O
      +->E<-+  

into this:

      ┌──●──┐  
    O◀│  │  │▶O
      ├─┬┼┬─┤  
      ├─┼┼┼─┤  
      ├─┴┼┴─┤  
    O◀│  │  │▶O
      └─▶E◀─┘  

The executable
--------------

When compiled the executable must be run at the root of the project because it will look for `valid*.txt` files into the `test` directory.
