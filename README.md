# Tex-like Preprocessor (TLPP)
TLPP is a code preprocessor with tex-like syntax and implemented in pure python. It can be used as a preprocessor of the code or as a macro-language like *Gnu M4*. Note, TLPP is **not** a programming language . Its development is not finished yet.

## Why I made this
The goal of this project is to make a literature programming system like WEB. This is for people, who want to share the ideal not only the code. For example, an open-source object shares the code. But only a very few people can get the ideals by reading the code. With this preprocessor, the code can look more like a natur-language. In fact you can add a suit of macro definition to generate .tex file and compile it to pdf file!

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
A macro of TLPP is called by ```\```, to call a macro named ```foo``` you should use
```\foo```
if ```\foo``` macro has two arguments, use
```\some arg1 arg2```
The ```arg1``` and ```arg2``` can only be single character and not space ```' '```. The white space between two arguments will be ignored. For example
```
\some a b
```
The macro gets ```a``` as first argument and ```b``` as second argument. If you want to use a set of characters as argument, you should give a ```group``` as argument. A ```group``` is enclosed by ```{}```. For our example:
```
\foo{arg1}{arg2}
```
the macro gets ```arg1``` and ```arg2``` as arguments.

Arguments can be placed befor the call of macro. For example if we have a macro ```
get``` takes one argument befor the calling and one after, use
```
{arg1}\get{arg2}
```
, again if ```arg1``` or ```arg2``` is single charactor (and not space), it is not necessory to use group. In this project we call the arguments which will be placed before the macro calling as **negative arguments** and after the macro calling as **positive arguments**.
## Process
The processor will scan the text to a list of ```Token``` object. Tokens have different catalogues, low level catalogues are
- ```letter``` includes ```[A-Za-z]``` and ```@```.
- ```leader``` includes ```\```.
- ```groupstart``` includes ```{```.
- ```groupend``` includes ```}```.
- ```textend``` includes ```\0``` the end character of a string as in C.
- ```space``` includes white space ```' '``` and tab ```\t```.
- ```nl``` includes ```\n``` and ```\r```.
- ```others``` includes all other characters.
And high level catagoures are
- ```macroname```: A macro call like ```\foo``` will be recognized as a single token. Note, the text storaged in the ```Token``` oblect will not include the backslash ```\```.
- ```group```: As before, text enclosed in ```{...}``` will be storaged as a single token.
- ```hash```: The processor will take this as a single token, but not expand it. At the end it will be replaced by the value storaged in the hash table. This is used to make an escape enviroment.

The processor will first "see" the ```leader``` then start to find the macro. For arguments, tokens will catalogues ````space, nl, textend``` and ```leader``` will not be considered.

## built-in macros
All built-in macros are start with ```@```. Which means they are fixed, more see below.
### ```\@newMacro{name}{body}```
Define a new macro, with name ```name``` and replace text ```body```. 
The ```name``` can only use characters with catalogue ```letter```. So it can only be letters or ```@```. If at least one ```@``` in the macro name, this macro is fixed. You can not delete it or change the defination. 
### ```\@callpy{func}arg\@endcallpy```
This will call the function ```func``` in your script and gives ```arg``` as the argument. When use command line, you can load your script via ```-s``` option. When you want use it in your programm, load the script as a ```module``` oject and put it in a ```dict```. You can put more than one module into the dict, and pass it into the ```scripts``` argument, when create the processor instance.

The function should take only one string argument. It can also take two, first one is the processor instance, second one is ```arg```. Thus you can interact with the processor. If you want to call this function, add ```@``` at the begin of the function name, like ```\@callpy{@func}arg\@endcallpy``` The return value should be a string or a tuple (also ```None```). If the return value is a string, it will become the expanding result. If it is ```None```, is same as return an empty string. If you return a tuple, the first element should have type ```int```, this is the key in the hash table. 2nd element should be a ```str``` or ```None```. If it is a string, it will be the value under the first key in hash table, if the key already exist, the value will changed to it. If it is ```None```, then it will not change the value, but if the key is new, the processor will create a new iterm with the key. The last element should be ```bool```. ```True``` if you want to put the text here (it will not change anymore, even later you changed the value in the hash table), ```False``` if you want to put a tag here, and at the end it will replaced by the value in the hash table.
### ```@hash{hashcode}```
As before, it will be used as a tag. It will not be expanded until the end. Then replaced by the value in the hash table. This macro will be storaged as a 
```hash``` Token. ```hashcode``` should be the key in hash table i.e. integer. You really should not use it directly by hand.

I want to keep a minimal number of built-in macros, with ```\@callpy``` you can really define what kind of macros you like.