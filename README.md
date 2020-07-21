# MIPS-Simulator-
Developed a lightweight interactive development environment (IDE) in C++ for programming in MIPS assembly language

The framework code
1.It reads the machine code into "memory", starting at "address" 0x00400000. (In keeping with the MARS convention, addresses from 0x0000000 to 0x00400000are unused.) We assume that the program will be no more than 1024 words long. The name of the Hile that contains the code is given as a command-line argument. 
2.It initializes the stack pointer to 0x00404000, it initializes all other registers to 0x00000000, and it initializes the program counter to 0x00400000. 
3.It provides simulated data memory starting at address 0x00401000 and ending at address 0x00404000. Internally, it stores instructions together with data in the same memory array. 
4.It sets flags that govern how the program interacts with the user. 

It then enters a loop that repeatedly fetches and executes instructions, printing information as it goes: 
•the machine instruction being executed, along with its address and disassembled form (to be supplied by your PrintInstruction function);
•the new value of the program counter; 
•information about the current state of the registers;
•information about the contents of memory.
