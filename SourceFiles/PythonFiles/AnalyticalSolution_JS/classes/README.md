
reason for this class structure is mentioned in 

https://stackoverflow.com/questions/47561840/how-can-i-separate-the-functions-of-a-class-into-multiple-files

This allows me to put the main class in the __init__ file as opposed to a
file for the class itself

For example, if import the DuctModeClass.py file and avoid using "DuctModeClass.DuctModeClass", 
I have to import it like this,

from classes.DuctModeClass import DuctModeClass

with the __init__ file, I can do this instead!
from classes import DuctModeClass
