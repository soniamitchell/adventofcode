# Start again with the original input image and apply the image enhancement algorithm 50 times
img <- enhance_image(input, 50, algorithm)

# How many pixels are lit in the resulting image?
sum(img == "#")
