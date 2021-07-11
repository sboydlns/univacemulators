# PACK

# op1 longer op2
SET MEM $10C=123
PACK $20(4),$10(3)
TEST MEM $20W=$0000123F
TEST MEM $1FB=0

# op1 same length as op2
SET MEM $10C=1234567
PACK $20(4),$10(7)
TEST MEM $20W=$1234567F
TEST MEM $1FB=0

# op1 shorter op2
SET MEM $10C=123456789
PACK $20(4),$10(9)
TEST MEM $20W=$3456789F
TEST MEM $1FB=0

# Swap nibbles in single byte
SET MEM $10B=$10
PACK $10(1),$10(1)
TEST MEM $10B=$01

# UNPK

# op1 longer than op2
SET MEM $10C=123
PACK $20(2),$10(3)
UNPK $30(4),$20(2)
TEST MEM $30C=0123
TEST MEM $2FB=0

# op1 same length as op2
SET MEM $10C=123
PACK $20(2),$10(3)
UNPK $30(3),$20(2)
TEST MEM $30C=123
TEST MEM $2FB=0

# op1 shorter op2
SET MEM $10C=12345
PACK $20(3),$10(5)
UNPK $30(3),$20(3)
TEST MEM $30C=345
TEST MEM $2FB=0

# overlap
SET MEM $10C=12345
PACK $20(3),$10(5)
UNPK $20(5),$20(3)
TEST MEM $20C=12345
TEST MEM $1FB=0

# MVO

# op1 longer op2
SET MEM $10C=123
SET MEM $30W=$FFFFFFFF
PACK $20(4),$10(3)
MVO $30(4),$22(2)
NI $33,$0F
TEST MEM $30W=$0001230F
TEST MEM $2FB=0

# op1 shorter op2
SET MEM $10C=12345
SET MEM $30W=$FFFFFFFF
PACK $20(4),$10(5)
MVO $30(2),$20(4)
NI $31,$0F
TEST MEM $30W=$450FFFFF
TEST MEM $2FB=0

# AP

# overflow
SET MEM $10W=$9999999C
SET MEM $14W=$0000001C
AP $10(4),$14(4)
TEST MEM $10W=$0000000C
TEST CC=3

# positive result
SET MEM $10W=$0000123C
SET MEM $14W=$0000234C
AP $10(4),$14(4)
TEST MEM $10W=$0000357C
TEST CC=2

# negative result
SET MEM $10W=$0000123C
SET MEM $14W=$0000124D
AP $10(4),$14(4)
TEST MEM $10W=$0000001D
TEST CC=1

# zero result
SET MEM $10W=$0000123C
SET MEM $14W=$0000123D
AP $10(4),$14(4)
TEST MEM $10W=$0000000C
TEST CC=0

# add to self
SET MEM $10W=$0000123C
AP $10(4),$10(4)
TEST MEM $10W=$0000246C
TEST CC=2
