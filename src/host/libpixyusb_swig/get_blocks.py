from pixy import *

blocks = Block()

# Pixy Python SWIG get blocks example #

print ("Pixy Python SWIG Example -- Get Blocks")

# Initialize Pixy Interpreter thread #
pixy_init()

# Wait for blocks #
while 1:
  count = pixy_get_blocks(20, blocks)
  if count > 0:
    # Blocks found #
    # TODO: Display blocks #
    print ("[%",count,"]")
