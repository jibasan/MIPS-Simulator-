#include <stdio.h>
#include <stdlib.h>
#include <netinet/in.h>
#include "computer.h"
#undef mips			/* gcc already has a def for mips */

/*
mnemonic/meaning/type/opcode/funct
addu	Add Unsigned	        R	0x00	0x21
addiu	Add Unsigned Immediate	I	0x09	NA
subu	Unsigned Subtract	    R	0x00	0x23
sll	    Logical Shift Left  	R	0x00	0x00
srl	    Logical Shift Right (0-extended)	R	0x00	0x02
and 	Bitwise AND	            R	0x00	0x24
andi	Bitwise AND Immediate	I	0x0C	NA
or	    Bitwise OR	R	0x00	0x25
ori	    Bitwise OR Immediate	I	0x0D	NA
lui	    Load Upper Immediate	I	0x0F	NA
slt	    Set to 1 if Less Than	R	0x00	0x2A
beq	    Branch if Equal	        I	0x04	NA
bne	    Branch if Not Equal	    I	0x05	NA
j	    Jump to Address	        J	0x02	NA
jal	    Jump and Link	        J	0x03	NA
jr	    Jump to Address in Register	R	0x00	0x08
lw	    Load Word	            I	0x23	NA
sw	    Store Word            	I	0x2B	NA
*/

unsigned int endianSwap(unsigned int);

void PrintInfo (int changedReg, int changedMem);
unsigned int Fetch (int);
void Decode (unsigned int, DecodedInstr*, RegVals*);
int Execute (DecodedInstr*, RegVals*);
int Mem(DecodedInstr*, int, int *);
void RegWrite(DecodedInstr*, int, int *);
void UpdatePC(DecodedInstr*, int);
void PrintInstruction (DecodedInstr*);

/*Globally accessible Computer variable*/
Computer mips;
RegVals rVals;

/*
 *  Return an initialized computer with the stack pointer set to the
 *  address of the end of data memory, the remaining registers initialized
 *  to zero, and the instructions read from the given file.
 *  The other arguments govern how the program interacts with the user.
 */
void InitComputer (FILE* filein, int printingRegisters, int printingMemory,
  int debugging, int interactive) {
    int k;
    unsigned int instr;

    /* Initialize registers and memory */

    for (k=0; k<32; k++) {
        mips.registers[k] = 0;
    }
    
    /* stack pointer - Initialize to highest address of data segment */
    mips.registers[29] = 0x00400000 + (MAXNUMINSTRS+MAXNUMDATA)*4;

    for (k=0; k<MAXNUMINSTRS+MAXNUMDATA; k++) {
        mips.memory[k] = 0;
    }

    k = 0;
    while (fread(&instr, 4, 1, filein)) {
	/*swap to big endian, convert to host byte order. Ignore this.*/
        mips.memory[k] = ntohl(endianSwap(instr));
        k++;
        if (k>MAXNUMINSTRS) {
            fprintf (stderr, "Program too big.\n");
            exit (1);
        }
    }

    mips.printingRegisters = printingRegisters;
    mips.printingMemory = printingMemory;
    mips.interactive = interactive;
    mips.debugging = debugging;
}

unsigned int endianSwap(unsigned int i) {
    return (i>>24)|(i>>8&0x0000ff00)|(i<<8&0x00ff0000)|(i<<24);
}

/*
 *  Run the simulation.
 */
void Simulate () {
    char s[40];  /* used for handling interactive input */
    unsigned int instr;
    int changedReg=-1, changedMem=-1, val;
    DecodedInstr d;
    
    /* Initialize the PC to the start of the code section */
    mips.pc = 0x00400000;
    while (1) {
        if (mips.interactive) {
            printf ("> ");
            fgets (s,sizeof(s),stdin);
            if (s[0] == 'q') {
                return;
            }
        }

        /* Fetch instr at mips.pc, returning it in instr */
        instr = Fetch (mips.pc);

        printf ("Executing instruction at %8.8x: %8.8x\n", mips.pc, instr);

        /* 
	 * Decode instr, putting decoded instr in d
	 * Note that we reuse the d struct for each instruction.
	 */
        Decode (instr, &d, &rVals);

        /*Print decoded instruction*/
        PrintInstruction(&d);

        /* 
	 * Perform computation needed to execute d, returning computed value 
	 * in val 
	 */
        val = Execute(&d, &rVals);

	UpdatePC(&d,val);

        /* 
	 * Perform memory load or store. Place the
	 * address of any updated memory in *changedMem, 
	 * otherwise put -1 in *changedMem. 
	 * Return any memory value that is read, otherwise return -1.
         */
        val = Mem(&d, val, &changedMem);

        /* 
	 * Write back to register. If the instruction modified a register--
	 * (including jal, which modifies $ra) --
         * put the index of the modified register in *changedReg,
         * otherwise put -1 in *changedReg.
         */
        RegWrite(&d, val, &changedReg);

        PrintInfo (changedReg, changedMem);
    }
}

/*
 *  Print relevant information about the state of the computer.
 *  changedReg is the index of the register changed by the instruction
 *  being simulated, otherwise -1.
 *  changedMem is the address of the memory location changed by the
 *  simulated instruction, otherwise -1.
 *  Previously initialized flags indicate whether to print all the
 *  registers or just the one that changed, and whether to print
 *  all the nonzero memory or just the memory location that changed.
 */
void PrintInfo ( int changedReg, int changedMem) {
    int k, addr;
    printf ("New pc = %8.8x\n", mips.pc);
    if (!mips.printingRegisters && changedReg == -1) {
        printf ("No register was updated.\n");
    } else if (!mips.printingRegisters) {
        printf ("Updated r%2.2d to %8.8x\n",
        changedReg, mips.registers[changedReg]);
    } else {
        for (k=0; k<32; k++) {
            printf ("r%2.2d: %8.8x  ", k, mips.registers[k]);
            if ((k+1)%4 == 0) {
                printf ("\n");
            }
        }
    }
    if (!mips.printingMemory && changedMem == -1) {
        printf ("No memory location was updated.\n");
    } else if (!mips.printingMemory) {
        printf ("Updated memory at address %8.8x to %8.8x\n",
        changedMem, Fetch (changedMem));
    } else {
        printf ("Nonzero memory\n");
        printf ("ADDR	  CONTENTS\n");
        for (addr = 0x00400000+4*MAXNUMINSTRS;
             addr < 0x00400000+4*(MAXNUMINSTRS+MAXNUMDATA);
             addr = addr+4) {
            if (Fetch (addr) != 0) {
                printf ("%8.8x  %8.8x\n", addr, Fetch (addr));
            }
        }
    }
}

/*
 *  Return the contents of memory at the given address. Simulates
 *  instruction fetch. 
 */
unsigned int Fetch ( int addr) {
    return mips.memory[(addr-0x00400000)/4];
}

/* Decode instr, returning decoded instruction. */
void Decode ( unsigned int instr, DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */

    d->op = instr >> 26; //declare left 6 bits cause every format has opcode

    //R-format - opcode/rs/rt/rd/shamt/funct 31:26/25:21/20:16/15:11/10:6/5:0
    //all R-format in this project uses 0x00 opcode
    if (d->op == 0){
        d->type = R;
        //rs = d->regs.r.rs
        rVals->R_rs = mips.registers[d->regs.r.rs] >> 21;
        //rt = d->regs.r.rt
        rVals->R_rt = mips.registers[d->regs.r.rt] >> 16;
        //rd = d->regs.r.rd
        rVals->R_rd = mips.registers[d->regs.r.rd] >> 11;

        //shamt
        d->regs.r.shamt = (instr & 0x000007c0) >> 6;

        //funct
        d->regs.r.funct = instr & 0x0000003f;
    }

    //J-format - opcode/address
    else if (d->op == 2 || d->op == 3) {
        d->type = J;
        d->regs.j.target = instr & 0x03ffffff;
            return;
    }

    //I-format - opcode/rs/rt/imm
    else {
        d->type = I;
        //rs = d->regs.r.rs
        rVals->R_rs = mips.registers[d->regs.r.rs >> 21];
        //rt = d->regs.r.rt
        rVals->R_rt = mips.registers[d->regs.r.rt] >> 16;

        d->regs.i.addr_or_immed = (short) (instr & 0x0000ffff);
    }

}

/*
 *  Print the disassembled version of the given instruction
 *  followed by a newline.
 */
void PrintInstruction ( DecodedInstr* d) {
    /* Your code goes here */

    int rs = d->regs.r.rs;
    int rt = d->regs.r.rt;
    int rd = d->regs.r.rd;
    int imm = d->regs.i.addr_or_immed;

    //R-format
    /*
mnemonic/meaning/type/opcode/funct
addu	Add Unsigned	        R	0x00	0x21                R[rd]=R[rs]+R[rt]
subu	Unsigned Subtract	    R	0x00	0x23                R[rd] = R[rs] - R[rt]
sll	    Logical Shift Left  	R	0x00	0x00                R[rd] = R[rt] << shamt
srl	    Logical Shift Right (0-extended)	R	0x00	0x02    R[rd] = R[rt] >> shamt
and 	Bitwise AND	            R	0x00	0x24                R[rd] = R[]rs] & R[rt]
or	    Bitwise OR	            R	0x00	0x25                R[rd] = R[rs]| R[rt]
slt	    Set to 1 if Less Than	R	0x00	0x2A                R[rd] = (R[rs]<R[rt])? 1:0
jr	    Jump to Address in Register	R	0x00	0x08            PC=R[rs]
*/
    switch (d->op){
        case 0:
            switch (d->regs.r.funct){

    //addu
    case 0x21:
    printf("addu %d\t $%d, $%d, \n", rd, rs, rt);
    break;

    //subu
    case 0x23:
    printf("subu %d\t $%d, $%d, \n", rd, rs, rt);
    break;

    //sll
    case 0x00:
    printf("sll ");
    break;

    //srl
    case 0x02:
    printf("srl %d\t $%d, $%d, \n", rd, rt, d->regs.r.shamt);
    break;

    //and;
    case 0x24:
    printf("and %d\t $%d, $%d, \n", rd, rs, rt);
    break;

    //or
    case 0x25:
    printf("or %d\t $%d, \n", rd, rs, rt);
    break;

    //slt
    case 0x2A:
    printf("slt %d\t $%d \n", rd, rs, rt);
    break;

    //jr
    case 0x8:
    printf("jr %d\t, \n", rs);
    break;
            }

    //

    //I-format
            /*
         addiu	Add Unsigned Immediate	I	0x09	NA                  R[rt] = R[rs] +SignExtImm
andi	Bitwise AND Immediate	I	0x0C	NA                  R[rt] = R[rs] & ZeroExtImm
ori	    Bitwise OR Immediate	I	0x0D	NA                  R[rt] = R[rs] | ZeroExtImm
lui	    Load Upper Immediate	I	0x0F	NA                  R[rt] = {imm, 16'b0}
beq	    Branch if Equal	        I	0x04	NA                  if(R[rs]==R[R[rt])
                                                            PC=Pc+4+BranchAddr
bne	    Branch if Not Equal	    I	0x05	NA                  if(R[rs]!=R[R[rt])
                                                            PC=Pc+4+BranchAddr
lw	    Load Word	            I	0x23	NA                  R[rt]=M[R[rs]+SignExtImm]
sw	    Store Word            	I	0x2B	NA                  M[R[rs]+SignExtImm]=R[rt]
         */

    //addiu
        case 0x09:
    printf("addiu \t$%d, $%d, %d\n ", rt, rs, imm);
    break;

    //andi
        case 0x0C:
    printf("andi \t$%d, $%d, %d\n", rt, rs, imm);
    break;

    //ori
        case 0x0D:
    printf("ori \t$%d, $%d, 0x%x\n", rt, rs, imm);
    break;

    //lui
        case 0x0F:
    printf("lui \t$%d, $%d, 0x%x\n" , rt, rs, (int) imm);
    break;

    //beq
        case 0x04:
    printf("beq \t$%d, $%d, 0x%08x\n", rs, rt, imm);
    break;

    //bne
        case 0x05:
    printf("bne \t$%d, $%d, 0x%08x\n", rs, rt, mips.pc + 4 + (imm << 2));
    break;

    //lw
        case 0x23:
    printf("lw \t$%d, %d($%d)\n", rt, imm, rs);
    break;

    //sw
        case 0x2B:
    printf("sw \t$%d, %d($%d)\n" , rt, imm, rs);
    break;

    //J-format
            //J-format
            /*
    j	    Jump to Address	        J	0x02	NA                  PC=JumpAddr
    jal	    Jump and Link	        J	0x03	NA                  R[31]=PC+8;PC=JumpAddr
                */
    //j
        case 0x02:
    printf("j");
    break;

    //jal
        case 0x03:
    printf("jal");
    break;
    }



}

/* Perform computation needed to execute d, returning computed value */
int Execute ( DecodedInstr* d, RegVals* rVals) {
    /* Your code goes here */

    //R-format
    /*
mnemonic/meaning/type/opcode/funct
addu	Add Unsigned	        R	0x00	0x21                R[rd]=R[rs]+R[rt]
subu	Unsigned Subtract	    R	0x00	0x23                R[rd] = R[rs] - R[rt]
sll	    Logical Shift Left  	R	0x00	0x00                R[rd] = R[rt] << shamt
srl	    Logical Shift Right (0-extended)	R	0x00	0x02    R[rd] = R[rt] >> shamt
and 	Bitwise AND	            R	0x00	0x24                R[rd] = R[]rs] & R[rt]
or	    Bitwise OR	            R	0x00	0x25                R[rd] = R[rs]| R[rt]
slt	    Set to 1 if Less Than	R	0x00	0x2A                R[rd] = (R[rs]<R[rt])? 1:0
jr	    Jump to Address in Register	R	0x00	0x08            PC=R[rs]
*/

    switch (d->op) {
        case 0: //opcode is 0 for these R-format

            switch (d->regs.r.funct) { //reads the right 6 bits for the funct
                //addu
                case 0x21:
                    return rVals->R_rs + rVals->R_rt;

                    // subu
                case 0x23:
                    return rVals->R_rs - rVals->R_rt;

                    //sll
                case 0x00:
                    return rVals->R_rt << d->regs.r.shamt;

                    //srl
                case 0x02:
                    return rVals->R_rt >> d->regs.r.shamt;

                    //and
                case 0x24:
                    return rVals->R_rs & rVals->R_rt;

                    //or
                case 0x25:
                    return rVals->R_rs | rVals->R_rt;

                    //slt
                case 0x2A:
                    return rVals->R_rs < rVals->R_rt;

                    //jr
                case 0x08:
                    return rVals->R_rs;
            }


            //I-format
            /*
             addiu	Add Unsigned Immediate	I	0x09	NA                  R[rt] = R[rs] +SignExtImm
    andi	Bitwise AND Immediate	I	0x0C	NA                  R[rt] = R[rs] & ZeroExtImm
    ori	    Bitwise OR Immediate	I	0x0D	NA                  R[rt] = R[rs] | ZeroExtImm
    lui	    Load Upper Immediate	I	0x0F	NA                  R[rt] = {imm, 16'b0}
    beq	    Branch if Equal	        I	0x04	NA                  if(R[rs]==R[R[rt])
                                                                PC=Pc+4+BranchAddr
    bne	    Branch if Not Equal	    I	0x05	NA                  if(R[rs]!=R[R[rt])
                                                                PC=Pc+4+BranchAddr
    lw	    Load Word	            I	0x23	NA                  R[rt]=M[R[rs]+SignExtImm]
    sw	    Store Word            	I	0x2B	NA                  M[R[rs]+SignExtImm]=R[rt]
             */

            //addiu
        case 0x09:
            return rVals->R_rs + d->regs.i.addr_or_immed;

            //andi
        case 0x0C:
            return rVals->R_rs & d->regs.i.addr_or_immed;

            //ori
        case 0x0D:
            return rVals->R_rs | d->regs.i.addr_or_immed;

            //lui
        case 0x0F:
            return d->regs.i.addr_or_immed << 16;

            //beq
        case 0x04:
            return rVals->R_rs == rVals->R_rt;

            //bne
        case 0x05:
            return rVals->R_rs != rVals->R_rt;

            //lw
        case 0x23:
            return rVals->R_rs + d->regs.i.addr_or_immed;

            //sw
        case 0x2B:
            return rVals->R_rs + d->regs.i.addr_or_immed;

            //J-format
            /*
    j	    Jump to Address	        J	0x02	NA                  PC=JumpAddr
    jal	    Jump and Link	        J	0x03	NA                  R[31]=PC+8;PC=JumpAddr
                */
            //j
        case 0x02:
            return 0;

            //jal
        case 0x03:
            return mips.pc + 4;
    }

}

/* 
 * Update the program counter based on the current instruction. For
 * instructions other than branches and jumps, for example, the PC
 * increments by 4 (which we have provided).
 */
void UpdatePC ( DecodedInstr* d, int val) {
    mips.pc+=4;
    /* Your code goes here */

    if(d->type == R && d->regs.r.funct == 0x08){ //jr
        mips.pc = mips.registers[31];
        return;
    }
    if(d->type == I && (d->op == 0x04 || d->op == 0x5){ //beq and bne
        if (val) {
            mips.pc = val;
        }

    }
    if(d->type == J){ //j and jal
        mips.pc = val;
        return;
    }
    if(mips.pc < 0x00400000 || mips.pc > 0x00401000 || mips.pc % 4 != 0)
    {
        //if PC address is out of bounds, or is not word-aligned.
        exit(3);
    }
}

/*
 * Perform memory load or store. Place the address of any updated memory 
 * in *changedMem, otherwise put -1 in *changedMem. Return any memory value 
 * that is read, otherwise return -1. 
 *
 * Remember that we're mapping MIPS addresses to indices in the mips.memory 
 * array. mips.memory[0] corresponds with address 0x00400000, mips.memory[1] 
 * with address 0x00400004, and so forth.
 *
 */
int Mem( DecodedInstr* d, int val, int *changedMem) {
    /* Your code goes here */
    if ((d->op == 0x23 || d->op == 0x2B) && ( val % 4 !=0|| val < 0x00401000 ||val > 0x00403fff ))
    {
        printf("Memory Access Exception at 0x%.8x: address 0x%.8x", mips.pc - 4, val);
        exit(2);
    }
    *changedMem = -1;

    switch (d->op)
    {

        // lw
        case 0x23:

            return mips.memory[(val - 0x00400000) >> 2]; //returns value at mem[address]
            // sw
        case 0x2B:

            mips.memory[((*changedMem = val) - 0x00400000) >> 2] = mips.registers[d->regs.i.rt];

            break;
    }
    return val;   //else return val

}

/* 
 * Write back to register. If the instruction modified a register--
 * (including jal, which modifies $ra) --
 * put the index of the modified register in *changedReg,
 * otherwise put -1 in *changedReg.
 */
void RegWrite( DecodedInstr* d, int val, int *changedReg) {
    /* Your code goes here */
    switch (d->op) {
        case 0:
            switch (d->regs.r.funct){
                    //addu
                case 0x21:

                    // subu
                case 0x23:

                    //sll
                case 0x00:

                    //srl
                case 0x02:

                    //and
                case 0x24:

                    //or
                case 0x25:

                    //slt
                case 0x2A:
                    *changedReg = d->regs.r.rd;
                    mips.registers[d->regs.r.rd] = val;
                    return;

            }
            return;

            // jal
        case 0x03:
            *changedReg = 31;
            mips.registers[31] = val;
            return;
            // addiu
        case 0x09:
            // andi
        case 0x0C:
            // ori
        case 0x0D:
            // lui
        case 0x0f:
            // lw
        case 0x23:
            *changedReg = d->regs.i.rt;
            mips.registers[d->regs.i.rt] = val;
            return;
    }
}
