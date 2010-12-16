## Demos

Assume that all demos are executed with the working directory in the same folder.

## Layers Order

1. background
    * things that shouldn't change
    * grid, axes, title
2. persistent layer
    * mutaframe
      * data, colors, size, etc.
3. transient layer
    * temporary brushing, colors, etc.
4. NULL (empty)
5. others can be used for special plots.


