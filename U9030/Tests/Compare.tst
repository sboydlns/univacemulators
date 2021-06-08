#
# Comaprison instructions test
#

# C

# Positive operands equal
SET R1=$01000000
SET MEM $10W=$01000000
C R1,$10
TEST CC=0

# Negative operands equal
SET R1=-1
SET MEM $10W=-1
C R1,$10
TEST CC=0

# Positive op1 > op2
SET R1=2
SET MEM $10W=1
C R1,$10
TEST CC=2

# Negative op1 > op2
SET R1=-1
SET MEM $10W=-2
C R1,$10
TEST CC=2

# Positive op1 < op2
SET R1=1
SET MEM $10W=2
C R1,$10
TEST CC=1

# Negative op1 < op2
SET R1=-2
SET MEM $10W=-1
C R1,$10
TEST CC=1

# Mixed op1 < op2
SET R1=-1
SET MEM $10W=1
C R1,$10
TEST CC=1

#Mixed op1 > op2
SET R1=1
SET MEM $10W=-1
C R1,$10
TEST CC=2

# CH

# Positive operands equal
SET R1=$0100
SET MEM $10H=$0100
CH R1,$10
TEST CC=0

# Negative operands equal
SET R1=-1
SET MEM $10H=-1
CH R1,$10
TEST CC=0

# Positive op1 > op2
SET R1=2
SET MEM $10H=1
CH R1,$10
TEST CC=2

# Negative op1 > op2
SET R1=-1
SET MEM $10H=-2
CH R1,$10
TEST CC=2

# Positive op1 < op2
SET R1=1
SET MEM $10H=2
CH R1,$10
TEST CC=1

# Negative op1 < op2
SET R1=-2
SET MEM $10H=-1
CH R1,$10
TEST CC=1

# Mixed op1 < op2
SET R1=-1
SET MEM $10H=1
CH R1,$10
TEST CC=1

#Mixed op1 > op2
SET R1=1
SET MEM $10H=-1
CH R1,$10
TEST CC=2

# CL

# Operands equal
SET R1=$01000000
SET MEM $10W=$01000000
CL R1,$10
TEST CC=0

# Negative operands equal
SET R1=-1
SET MEM $10W=-1
CL R1,$10
TEST CC=0

# Positive op1 > op2
SET R1=2
SET MEM $10W=1
CL R1,$10
TEST CC=2

# Negative op1 > op2
SET R1=-1
SET MEM $10W=-2
CL R1,$10
TEST CC=2

# Positive op1 < op2
SET R1=1
SET MEM $10W=2
CL R1,$10
TEST CC=1

# Negative op1 < op2
SET R1=-2
SET MEM $10W=-1
CL R1,$10
TEST CC=1

# Mixed op1 < op2
SET R1=1
SET MEM $10W=-1
CL R1,$10
TEST CC=1

#Mixed op1 > op2
SET R1=-1
SET MEM $10W=1
CL R1,$10
TEST CC=2

# CLI

# Operands equal
SET MEM $10B=1
CLI $10,1
TEST CC=0

# Negative operands equal
SET MEM $10B=-1
CLI $10,-1
TEST CC=0

# Positive op1 > op2
SET MEM $10B=2
CLI $10,1
TEST CC=2

# Negative op1 > op2
SET MEM $10B=-1
CLI $10,-2
TEST CC=2

# Positive op1 < op2
SET MEM $10B=1
CLI $10,2
TEST CC=1

# Negative op1 < op2
SET MEM $10B=-2
CLI $10,-1
TEST CC=1

# Mixed op1 < op2
SET MEM $10W=1
CLI $10,-1
TEST CC=1

#Mixed op1 > op2
SET MEM $10B=-1
CLI $10,1
TEST CC=2

# CLR

# Operands equal
SET R1=$01000000
SET R2=$01000000
CLR R1,R2
TEST CC=0

# Negative operands equal
SET R1=-1
SET R2=-1
CLR R1,R2
TEST CC=0

# Positive op1 > op2
SET R1=2
SET R2=1
CLR R1,R2
TEST CC=2

# Negative op1 > op2
SET R1=-1
SET R2=-2
CLR R1,R2
TEST CC=2

# Positive op1 < op2
SET R1=1
SET R2=2
CLR R1,R2
TEST CC=1

# Negative op1 < op2
SET R1=-2
SET R2=-1
CLR R1,R2
TEST CC=1

# Mixed op1 < op2
SET R1=1
SET R2=-1
CLR R1,R2
TEST CC=1

#Mixed op1 > op2
SET R1=-1
SET R2=1
CLR R1,R2
TEST CC=2

# CR

# Positive operands equal
SET R1=$01000000
SET R2=$01000000
CR R1,R2
TEST CC=0

# Negative operands equal
SET R1=-1
SET R2=-1
CR R1,R2
TEST CC=0

# Positive op1 > op2
SET R1=2
SET R2=1
CR R1,R2
TEST CC=2

# Negative op1 > op2
SET R1=-1
SET R2=-2
CR R1,R2
TEST CC=2

# Positive op1 < op2
SET R1=1
SET R2=2
CR R1,R2
TEST CC=1

# Negative op1 < op2
SET R1=-2
SET R2=-1
CR R1,R2
TEST CC=1

# Mixed op1 < op2
SET R1=-1
SET R2=1
CR R1,R2
TEST CC=1

#Mixed op1 > op2
SET R1=1
SET R2=-1
CR R1,R2
TEST CC=2

