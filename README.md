# Simple-Interpreter-CWRU-EECS345

Written in functional programming language scheme.  

This project is to create an interpreter for a very simple Java/C-ish language. The language has variables, assignment statements, mathematical expressions, comparison operators, boolean operators, if statements, while statements, and return statements.   
An example program is as follows:   
```java
var x;  
x = 10;
var y = 3 * x + 5;
while (y % x != 3)
  y = y + 1;
if (x > y)
  return x;
else if (x * x > y)
  return x * x;
else if (x * (x + x) > y)
  return x * (x + x);
else
  return y - 1;
```

The parser supports nested assignment statements as well as assignments inside expressions. The interpreter is written so that assignment operators return a value as well as initialize a variable: 
```java
var x;
var y;
x = y = 10;
if ((x = x + 1) > y)
  return x;
else
  return y;
```

The parser is provided by the instructor.   
