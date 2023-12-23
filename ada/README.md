# Ulam Spiral in ADA
## Environment

The scripts were written using Ada 2020. The project dependencies and build environment are handled by [Alire](https://alire.ada.dev/). To set up the environment:

1. Set up Alire in your system
2. Once alr is set up,run `alr update` to install the dependencies, or `alr run` to instal, compile, and run the program

## How to run the scrips
Once all the dependencies are satisfied, the script can be executed on console by using `alr run -a N` where N is the number size of the desired square. For example, `alr run -a 2` will create a $2\times2$ matrix. 

## Unit Test
This version include  a project for Unit Testing. Inside the `tests` folder in the main project. This is another Alire project that includes another set of dependencies for the code, as well as execute the test. 

To run them, just go to the `test` folder and execute `alr run`. It will download, compile and run the project automatically. 