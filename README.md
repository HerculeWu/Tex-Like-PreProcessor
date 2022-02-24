# Tex-like Preprocessor (TLPP)
TLPP is a code preprocessor with tex-like syntax and implemented in pure python. It can be used as a preprocessor of the code or as a macro-language like *Gnu M4*. Note, TLPP is **not** a programming language . Its development is not finished yet.
## Requirement
Pyhton 3.x (tested for 3.8)
**No** other packages!
## Usage
First download the project. Then in the path use command
```
python TLPP.py -h
```
to see how to process a file. If you want to use TLPP as a part of your project. You can copy the file ```processor.py``` to your project directory or in your ```PATH```. Then follow the example:
```
from TLPP import Processor
test_text = '\newMacro{hello}{hello world!}\hello'
processor = Processor(test_text)
canexpandmore = processor.scan()
print(processor.result)
```
The ```canexpandmore``` variable should be false. It means no more macro can be expanded.
## Basic Syntax
A macro of TLPP is called by ```\```, to call a macro named ```some``` you should use
```\some```
if ```\some``` macro has two arguments, use
```\some arg1 arg2```
The ```arg1``` and ```arg2``` can only be single character and not space ```' '```. The white space between two arguments are not necessory, they will be ignored. For example
```
\someab
```
is valid, if you don't have a macro named ```someab```. The macro gets ```a``` as first argument and ```b``` as second argument. But it is recomand to put white spaces, for readability. If you want to use a set of characters as argument, you should give a ```group``` as argument. A ```group``` is enclosed by ```{}```. For our example:
```
\some{arg1}{arg2}
```
the macro gets ```arg1``` and ```arg2``` as arguments.

Arguments can be placed befor the call of macro. For example if we have a macro ```
get``` takes one argument befor the calling and one after, use
```
{arg1}\get{arg2}
```
, again if ```arg1``` or ```arg2``` is single charactor (and not space), it is not necessory to use group.
## built-in macros
### ```\newMacro{name}{body}```
Define a new macro. ```name``` can start with ```\``` or not. But to call this new macro, you must use ```\```. Follow characters can not be used for a macro name
 - ```' '```(white space).
 - ```'\'```. You can put it at the begin, if you like. But it can not be a part of the macro name.
 - ```'{'``` and ```'}'```.
 - ```'\n'```(new line character).

When you want to use arguments, use```#```+number in ```body```. For example if we want to defnine macro ```\add``` with two arguments then use
```\newMacro{add}{#1 + #2}```
the processor will detect how many arguments there are (numbers must be in order so ```\newMacro{add}{#1 + #5}``` will not work, processor will assum there are only one argument). For arguments which need to put before the macro calling, use ```#-1, #-2```..., for example
```
\newMacro{add}{(#-1) + (#1)}
{a*b}\add{c*d}
```
The second line will expand as ```(a*b) + (c*d)```. The placeholder ```#0``` is for the macro name it self, without backslash. For example
```
\newMacro{operater}{#-1 #0 #1}
a \operator b
```
The second line will expand as ```a operator b```
### ```\renewMacro{name}{new_body}```
Like ```\newMacro```, used for overwrite the existed user macro. All built-in macros and blocked macros can not be overwritten.
### ```\newLineCharOff```
Ignore the new line charactor ```\n``` in the input document. By default it will not be ignored.
### ```\nl```
Add a new line character ```\n```. It can be used to make a line break, after ```\newLineCharOff```.
### ```\newLineCharOn```
The line break in the input document will be considered after this macro.

**Note:** for macro expanding, the line break will be considered by the currend situation.

### ```\include{file}```
Copy and pass the content of another file, which located at ```file``` to the position. Used for splite a big project into multi-file.
### ```\input{file}```
It works like ```\include{file}```. But it will first processes contents of ```file``` then put the result in the position. ```file``` will be expanded in a sparated processor. So all macro definitions inside ```file``` will not valid in the parent file and all macro definitions in the parent file, will not valid in ```file```. It can be used for a large project, which conflict of macro names becomes a big issue.

***Note***: If you use ```TLPP.py``` from command line, you can use ```--load``` or ```-l``` option to load a file. This file will add no content to the output file, but all macro definitions will be hold and valid globally, even for ```\input```.
### ```\quote text \endquote```
As an 'escape' enviroment. All contents in ```text``` will directly output to the result even there are macros inside. Note: ```\quote #1 \endquote``` inside a macro definition will not work like a normal quotation. ```#1``` will be still replaced by the actual arguments. But it will not expand further. For example
```
\newMacro{add}{#1 + \quote#2\endquote}
\newMacro{\bar}{a}
\newMacro{\foo}{b}
\add{\bar}{\foo}
```
The last line will first expand to ```\bar + \quote\foo\endquote``` then ```\bar``` will expand as a normal macro, but ```\quote\foo\endquote``` will be a quotation, so at the end you will have ```a + \foo``` as final result.

The first ```\endquote``` after ```\quote``` will be used. For example
```
\quote\quote\endquote
\quote\endquote\endquote
```
The first line works, it gives you ```\quote``` as the final result. But for the second line, at the first ```\endquote``` the quotation enviroment is already closed, so the second ```\endquote``` leads to an error. If you really want to output ```\endquote```, you can use
```
\newMacro{macroname}{\quote\\endquote#1}
\macroname{endquote}
```

Currently, if you wrapppe the quotation enviroment to a single macro, like
```
\newMacro{quota}{\quote#1\endquote}
```
it works, but if you want to use it like
```
\quota{{}
```
It leads to an error, because the second ```{``` is still the symbol for start a group for the processor due to ```\quota``` is just a normal macro, not an quotation enviroment!

In summary, I recomand to only use ```\quote...\endquote``` to add ```\```, ```{``` and ```}``` in the output.
### ```\ignore text \endignore```
Ignore the content in ```text```. Used for comment. It has same issue as in ```\quote..\endquote```.

### ```\callpy{functionname}\pyarg text \endpyarg```
When you use ```TLPP.py``` in command line, you can use ```--script``` or ```-s``` option to load a python script. All functions in the script can be called via this macro. All functions can only take one argument with type of string, which will be given as ```text``` between ```\pyarg``` and ```\endpyarg```. The return value of the function is the replace text of this statement. **Note**: The return value can be an empty string, but can not be ```None```. The script valid for all files in your project.

## blocked macros
Blocked macros can not be called by user. Also you can not redefine them. They will help the processing. Currently only ```\quotation{hashCode}``` is a blocked macro. It helps the processor to deal with quotation in recursive expanding. The content between ```\quote...\endquote``` will be put in to a hash table, and expand first as ```\quotation{hashCode}```. At the end this macro calling will be replaced by the value of ```hasCode``` in the hash table.

For the processor, the script will be loaded as a ```module``` object.

## TODO
- add support for single line comment
- add support for enviroment, like in latex
- add support for processor variable and operation of it. (Done by ```\callpy```. But still need when I want rewrite the processor in other language.)
- add more debug message
- make a position map between output text and input text
- more.....
