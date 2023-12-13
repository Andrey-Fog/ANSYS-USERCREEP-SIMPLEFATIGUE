# ANSYS-USERCREEP-SIMPLEFATIGUE
Simple creep-fatigue interaction law realized via ANSYS usercreep subroutine

<br>

 In source files you can find a APDL script example for 2D plate with hole. After compiling and attaching present dynamic link library as ANSYS user creep law copy and run [this file](https://github.com/Andrey-Fog/ANSYS-USERCREEP-SIMPLE_FATIGUE/blob/main/ANSCMD.txt) from ANSYS Mechanical command line.  

If you using this code for research or industrial purposes please cite:
[A.V. Tumanov et al. Crack growth rate prediction based on damage accumulation functions for creep-fatigue interaction](https://doi.org/10.3221/IGF-ESIS.52.23)  

Also you can find a [Visual abstract](https://www.youtube.com/watch?v=sueQTVURYUc) for reference paper.




<br>
<br>
<br>

## Instructions for compiling and attaching USERMATLIB.DLL 

Install Visual Studio first and then Intel fortran compiler. When installing the compiler, select "Integrate into Visual Studio". Supported versions and more information can be found in the ANSYS documentation in the section on User Programmable Features (UPF). Set LIB and INCLUDE variables in the system environment for your ANSYS version. Create new solution and add new fortran dll project. The name of the created library must be "USERCREEPLIB.DLL". Add all fortran files from Source directory to your dll project. Tune compiler according to instructions present below. After compiling you must connect this library to ANSYS.


<br>

### Connecting to ANSYS

---

After creation the dll file you have to connect this library to ANSYS:

<br>

**1. Create environment variable named ANS_USER_PATH**

*My Computer->Properties->Advanced system settings->Advanced*  

On the tab, click on the button:

*Environment Variables->System Variables->New*

<br>

**2. In the variable value field, specify the path to the folder where library is located. Use only latin characters in the path.**

*For example:* 
>C:\Users\Username\......\Usercreeplib
   
If everything is connected correctly in the ANSYS output window at startup there will be a line 

```
User link path <ANS_USER_PATH>: *path to your folder*" 
```
<br>

**3. After launching the ANSYS, create an user material**

*Preprocessor->Material Props->Material models*

<br>

**4. In the drop-down list of materials, select**

*Structural->Specialized Materials->User material options->user creep*


And add cells. There should be 10 properties in total. Of which:

| NN  |     | Property                              |
| --- | --- | ------------------------------------  |
|  C1 |  -  |Norton law const 1                      |
|  C2 |  -  |Norton law const 2                      |
|  C3 |  -  |Damage law const 1                      | 
|  C4 |  -  |Damage law const 2                     |  
|  C5 |  -  |Young modulus                          |
|  C6 |  -  |Fracture tougthness                    |
|  C7 |  -  |Hold time for trapezoidal cycle in seconds                                             | 
|  C8 |  -  |True ultimate stress                   |  
|  C9 |  -  |exponent of the linear part of s-n curve                           |
|  C10 |  -  |default is zero (1 - fatigue damage is disabled, 2 - creep damage is disabled)                    |

Also you can add this model from command line. The ANSYS APDL script will be looks like present bellow:

>!* Define parameters related to usercreep model  
>!* Norton law const   
>NortB	= 2.6e-15   
>!* Norton law power 
>NortN	= 1.93  
>!* Rabotnov damage law const  
>RbtnC	= 1.2e-12  
>!* Rabotnov damage law power 
>RbtnM 	= 2.5 
>!* Young modulus  
>YOUNG	= 120000  
>!* Fracture tougthness  
>K1C 	= 49
>!* Hold time for trapezoidal cycle in seconds  
>HOLDTIME 	= 60
>!* True ultimate stress  
>SULT 	= 49
>!* Exponent of the linear part of s-n curve  
>BEXP 	= -0.054
>!* Creep-fatigue flag  
>CFTYPE 	= 0
>     
>!***ADD USERCREEP LAW
>TB,CREE,1,1,10,100   
>TBTEMP,0
>TBDATA,,NortB,NortN,RbtnC,RbtnM,YOUNG,K1C   
>TBDATA,,HOLDTIME,SULT,BEXP,CFTYPE,, 

**5. Add 3 state variables**  

*Preprocessor->Material Props->Material models->Structural->Specialized Materials->User material options->State Variables*

| SVAR| Value                                 |
| --- | ------------------------------------- |
| 1   | Summary damage                        |
| 2   | temponary for debug purposes          |
| 3   | temponary for debug purposes          |


APDL script for preprocessor section

>TB,STAT,1,1,3,  
>TBTEMP,0  
>TBDATA,,0,0,0 


**6. Access to user variable arrays**

Before starting on the solution in the solver (/SOL) in the command line of ANSYS, write the line:

- to save every substeps results  
> OUTRES,SVAR,ALL

- to save only the last step  
> OUTRES,SVAR,LAST

That's all. Further we work as with the usual scheme.

<br>

### Fortran project tuning

For debug this library tune your compilator and linker according following table. If you don't want change the source code disable generation of debug information in both sections.
#### COMPILATOR SETTINGS (*projectname*->properties). 


| Name     |   | Value |
| ----------- | ----------- |----------- |
|Supress startup banner:| 	 - |	            Yes (/nologo) | 
|Additional include Directories:|  - |	        C:\Program Files\ANSYS Inc\v***\ansys\customize\include | 
|Optimization:| 			 - |	            Disable (/Od) |
|Preprocessor definitions:| 	 - |	        /DNOSTDCALL /DARGTRAIL /DPCWIN64_SYS /DPCWINX64_SYS /DPCWINNT_SYS /DCADOE_ANSYS /D__EFL /DFORTRAN /auto /c /Fo.\ /MD /W0  |
|Debug information Format:|		 - |            Full (/debug:full)  |
|Preprocess Source file:|		 - |            Yes (/fpp)  |
|Preprocessor Definitions to fpp only:| -|	Yes (/noD)  |
|Use Portlib Library:| 		 - |	            Yes (/4Yportlib)  |

#### LINKER SETTINGS  
| Name    |  |    Value |
| ----------- | ----------- |----------- |
|Enable incremental linking:| - |		No (/INCREMENTAL:NO)  |
|Supress startup banner:|  - |		    Yes (/nologo) | 
|Additional library Directories:|  - |	C:\Program Files\ANSYS Inc\v***\ansys\custom\lib\winx64|  
|Additional dependencies:| 	 - |	    ANSYS.LIB  |
|Generate debug info: |	 - |		    Yes (/DEBUG)  |

*** - your version of ANSYS.  
All other settings by default. Its allows me connect to ANSYS for debugging.
