
# 快速浏览：Racket图文教程
by Matthew Flatt


本教程通过使用Racket编程语言的一个绘图库对Racket编程语言进行简要介绍。这些例子很有趣，也很有启发作用，即使你不打算深入下去也值得一看。毕竟，一张照片抵得上500个“你好，世界”。

我们假设您将使用DrRacket运行示例。使用DrRacket是了解语言和系统感觉的最快方法，即使您最终在Emacs、vi或其他编辑器中使用Racket。

**1. 准备**

下载Racket，安装，然后启动DrRacket。

**2. 设置**

有关DrRacket IDE的简要概述，请参阅DrRacket文档。

为了绘制图片，我们必须首先加载一些图片处理函数，这些函数是幻灯片演示库的一部分。将以下内容复制到DrRacket窗口顶部，也就是定义区:

```
#lang slideshow
```

然后单击Run按钮，你将看到渔村移动到了底部文本区域，即交互区域。

如果你以前使用过DrRacket，你可能需要通过菜单中的语言|选择语言，选择“使用源中声明的语言”来重新设置DrRacket使用的语言。

**3. 开始**

当您在交互窗口中的>后面键入表达式并按回车时，DrRacket将计算该表达式并打印其结果。表达式可以只是一个值，比如数字5或字符串“art gallery”:

```
> 5
5
> "art gallery"
"art gallery"
```

表达式也可以是函数调用。要调用一个函数，在函数名左边加上一个左括号，右边是函数参数，然后是右括号，就像这样:

```
> (circle 10)
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict.png)

circle函数的结果是一张图片，它打印的方式与数字或字符串打印的方式非常相似。circle的参数是圆的半径，单位为像素，决定圆圈的大小。你可能猜到了，有一个矩形函数，它有两个参数而不是一个:

```
> (rectangle 10 20)
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_2.png)

试着给circle错误数量的参数，看看会发生什么:

```
> (circle 10 20)

 circle: arity mismatch;
 the expected number of arguments does not match the given number
  expected: 1 plus optional arguments with keywords #:border-color and #:border-width
  given: 2
  arguments...:
```

请注意，DrRacket用粉色高亮显示触发错误的表达式(但本文档中没有显示粉色高亮)。
除了像圆形和矩形这样的基本图片构造函数外，还有一个组合图片的hc-append函数。当你开始编写Racket函数调用，它看起来是这样的:

```
> (hc-append (circle 10) (rectangle 10 20))
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_3.png)

hc-append名称中的连字符只是标识符的一部分;它不是hc - append。函数名以h开头，因为它水平地组合了图片，下一个字母是c，因为图片是垂直居中的。

如果你想知道还有什么其他的功能——也许是一种垂直和左对齐堆叠图片的方法?将文本插入符号移动到名称hc-append，并在DrRacket中按F1键。将打开一个浏览器窗口，它将提供指向hc-append文档的链接。点击链接，您将看到许多其他功能。

**4. 定义**

要多次使用特定的圆形和矩形，更简单的方法是为它们命名。回到定义区域(顶部区域)并添加两个定义，这样定义区域的完整内容如下所示:

```
#lang slideshow
(define c (circle 10))
(define r (rectangle 10 20))
```

然后再次单击Run。现在，你可以输入c或r:

```
> r
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_4.png)
```
> (hc-append c r)
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_5.png)
```
> (hc-append 20 c r c)
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_6.png)

可以看到，hc-append函数在图片参数之前接受一个可选的数字参数，并且接受任意数量的图片参数。当提供一个数字时，它表示图片之间的间距。

我们可以在交互区域使用define语句定义c和r。但是在实践中，定义区域一般是我们写的程序文件，而交互区域用于临时查看和调试。
让我们定义一个函数，和定义形状一样，函数定义使用define，但要使用圆括号把函数名和参数括起来:

```
(define (square n)
      ; A semi-colon starts a line comment.
      ; The expression below is the function body.
      (filled-rectangle n n))
```

调用函数与定义函数的语法是一样的:

```
> (square 10)
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_7.png)

不管是交互区还是定义区，上面的表达式都能被执行。当程序运行时，来自定义区域的表达式结果显示在交互区域中。从现在开始，我们示例中的定义和表达式写在一起，因为这些示例将相互构建。

**5. 本地绑定**

定义表单可以在某些地方用于创建本地绑定。例如，它可以在函数体中使用:


```
(define (four p)
      (define two-p (hc-append p p))
      (vc-append two-p two-p))

     
    > (four (circle 10))
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_8.png)

但Racket程序员一般使用let或let*实现本地绑定。let的好处是它可以用于任何表达式位置。而且可以一次绑定多个定义，而不需要为每个定义使用一个define:


```
(define (checker p1 p2)
      (let ([p12 (hc-append p1 p2)]
            [p21 (hc-append p2 p1)])
        (vc-append p12 p21)))

     
    > (checker (colorize (square 10) "red")
               (colorize (square 10) "black"))
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_9.png)


A let form binds many identifiers at the same time, so the bindings cannot refer to each other. The let* form, in contrast, allows later bindings to use earlier bindings:

let可以一次绑定多个定义，但这些定义不能相互引用。而let*则允许后面的定义使用前面的定义:

```
(define (checkerboard p)
      (let* ([rp (colorize p "red")]
             [bp (colorize p "black")]
             [c (checker rp bp)]
             [c4 (four c)])
        (four c4)))

     
    > (checkerboard (square 10))
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_10.png)

**6. 函数也是值**

不要把circle作为一个函数调用，而是把circle作为一个表达式来计算:

```
> circle

    #<procedure:circle>
```

That is, the identifier circle is bound to a function (a.k.a. “procedure”), just like c is bound to a circle. Unlike a circle picture, there’s not a simple way of completely printing the function, so DrRacket just prints #<procedure:circle>.

This example shows that functions are values, just like numbers and pictures (even if they don’t print as nicely). Since functions are values, you can define functions that accept other functions as arguments:

也就是说，标识符circle被绑定到一个函数(也称为“过程”)，就像c被绑定到一个圆一样。与圆形图不同，没有一种简单的方法可以完全打印函数，所以dr球拍只打印#<procedure:circle>。
这个例子显示了函数是值，就像数字和图片一样(即使它们不能很好地打印)。因为函数是值，你可以定义函数接受其他函数作为参数:

    (define (series mk)
      (hc-append 4 (mk 5) (mk 10) (mk 20)))

     
    > (series circle)

    image
    > (series square)

    image

When calling a function that accepts a function argument, the argument function often isn’t needed anywhere else. Having to write down the function via define would be a hassle, because you have to make up a name and find a place to put the function definition. The alternative is to use lambda, which creates an anonymous function:

    > (series (lambda (size) (checkerboard (square size))))

    image

The parenthesized names after a lambda are the arguments to the function, and the expression after the argument names is the function body. Using the word “lambda” instead of “function” or “procedure” is part of Racket’s history and culture.

A define form for a function is really a shorthand for a simple define using lambda as the value. For example, the series definition could be written as

    (define series
      (lambda (mk)
        (hc-append 4 (mk 5) (mk 10) (mk 20))))

Most Racketeers prefer to use the shorthand function form with define instead of expanding to lambda.

**7. Lexical Scope**

Racket is a lexically scoped language, which means that whenever an identifier is used as an expression, something in the textual environment of the expression determines the identifier’s binding. This rule applies to identifiers in a lambda body as well as anywhere else.

In the following rgb-series function, the uses of mk in each lambda form refer to the argument of rgb-series, since that’s the binding that is textually in scope:

    (define (rgb-series mk)
      (vc-append
       (series (lambda (sz) (colorize (mk sz) "red")))
       (series (lambda (sz) (colorize (mk sz) "green")))
       (series (lambda (sz) (colorize (mk sz) "blue")))))

     
    > (rgb-series circle)

    image
    > (rgb-series square)

    image

Here’s another example, where rgb-maker takes a function and returns a new one that remembers and uses the original function.

    (define (rgb-maker mk)
      (lambda (sz)
        (vc-append (colorize (mk sz) "red")
                   (colorize (mk sz) "green")
                   (colorize (mk sz) "blue"))))

     
    > (series (rgb-maker circle))

    image
    > (series (rgb-maker square))

    image

Note how composing functions via rgb-maker creates a different alignment of objects within the picture compared to using rgb-series.

**8. Lists**

Racket inherits much of its style from the language Lisp, whose name originally stood for “LISt Processor,” and lists remain an important part of Racket.

The list function takes any number of arguments and returns a list containing the given values:

    > (list "red" "green" "blue")

    '("red" "green" "blue")
    > (list (circle 10) (square 10))

    '(image image)

As you can see, a list prints as a single quote and then pair of parentheses wrapped around the printed form of the list elements. There’s room for confusion here, because parentheses are used for both expressions, such as (circle 10), and printed results, such as '("red" "green" "blue"). The quote is the key difference, as discussed elsewhere. To help emphasize the difference, in the documentation and in DrRacket, result parentheses are printed in blue, unlike expression parentheses.

If you have a list, then you’ll eventually want to do something with each of the elements. The map function takes a list and a function to apply to each element of the list; it returns a new list to combine the function’s results:

    (define (rainbow p)
      (map (lambda (color)
             (colorize p color))
           (list "red" "orange" "yellow" "green" "blue" "purple")))

     
    > (rainbow (square 5))

    '(image image image image image image)

Another function that works with lists is apply. Like map, it takes a function and a list, but a function given to apply should take all of the arguments at once, instead of each one individually. The apply function is especially useful with functions that take any number of arguments, such as vc-append:

    > (apply vc-append (rainbow (square 5)))

    image

Note that (vc-append (rainbow (square 5))) would not work, because vc-append does not want a list as an argument; it wants a picture as an argument, and it is willing to accept any number of them. The apply function bridges the gap between a function that wants many arguments and a list of those arguments as a single value.

**9. Modules**

Since your program in the definitions window starts with

    #lang slideshow

all of the code that you put in the definitions window is inside a module. Furthermore, the module initially imports everything from the module designated by slideshow, which exports picture-making functions as well as more commonly used functions such as list and map.

To import additional libraries, use the require form. For example, the library pict/flash provides a filled-flash function:

    (require pict/flash)

     
    > (filled-flash 40 30)

    image

Modules are named and distributed in various ways:

    Some modules are packaged in the Racket distribution or otherwise installed into a hierarchy of collections. For example, the module name pict/flash means “the module implemented in the file "flash.rkt" that is located in the "pict" collection.” When a module name includes no slash, then it refers to a "main.rkt" file.

    Some collections of modules are distributed as packages. Packages can be installed using the Install Package... menu item in DrRacket’s File menu, or they can be installed using the raco pkg command-line tool. For example, installing the "avl" package makes the avl module available.

    Packages can be registered at https://pkgs.racket-lang.org/, or they can be installed directly from a Git repository, web site, file, or directory. See Package Management in Racket for more information about packages.

                To save your definitions, use DrRacket’s Save Definitions menu item.

    Some modules live relative to other modules, without necessarily belonging to any particular collection or package. For example, in DrRacket, if you save your definitions so far in a file "quick.rkt" and add the line

        (provide rainbow square)

    then you can open a new tab or window in DrRacket, type the new program "use.rkt" in the same directory as "quick.rkt":

        #lang racket
        (require "quick.rkt")
        (rainbow (square 5))

    and when you run "use.rkt", a rainbow list of squares is the output. Note that "use.rkt" is written using the initial import racket, which does not supply any picture-making functions itself—but does provide require and the function-calling syntax.

Racketeers typically write new programs and libraries as modules that import each other through relative paths and collection-based paths. When a program or library developed this way seems useful to others, it can be registered as a package, especially if the implementation is hosted in a Git repository.

**10. Macros**

Here’s another library to try:

    (require slideshow/code)

     
    > (code (circle 10))

    [image]

Instead of a circle, the result is a picture of the code that, if it were used as an expression, would produce a circle. In other words, code is not a function, but instead a new syntactic form for creating pictures; the bit between the opening parenthesis with code is not an expression, but instead manipulated by the code syntactic form.

This helps explain what we meant in the previous section when we said that racket provides require and the function-calling syntax. Libraries are not restricted to exporting values, such as functions; they can also define new syntactic forms. In this sense, Racket isn’t exactly a language at all; it’s more of an idea for how to structure a language so that you can extend it or create entirely new languages.

One way to introduce a new syntactic form is through define-syntax with syntax-rules:

    (define-syntax pict+code
      (syntax-rules ()
        [(pict+code expr)
         (hc-append 10
                    expr
                    (code expr))]))

     
    > (pict+code (circle 10))

    [image]

This kind of definition is a macro. The (pict+code expr) part is a pattern for uses of the macro; instances of the pattern in a program are replaced by instances of the corresponding template, which is (hc-append 10 expr (code expr)). In particular, (pict+code (circle 10)) matches the pattern with (circle 10) as expr, so it is replaced with (hc-append 10 (circle 10) (code (circle 10))).

Of course, this sort of syntactic extension cuts both ways: inventing a new language can make it easier to say what you want, but harder for others to understand. As it happens, the developers of Racket are constantly giving talks and writing papers that involve Racket code, and it’s worthwhile for everyone who works on those products to know about code.

In fact, you might want to take a look at the source of this document. You’ll see that it starts with #lang, but otherwise doesn’t look a lot like Racket; nevertheless, we build this document by running its source as a Racket program. We have to use a lot more than syntax-rules to extend Racket’s syntax enough for writing documents, but Racket’s syntactic extension can take you a long way.

**11. Objects**

An object system is another example of a sophisticated language extension that is worth learning and using for Racket users. Objects are sometimes better than functions, even when you have lambda, and objects work especially well for graphical user interfaces. The API for Racket’s GUI and graphics system is expressed in terms of objects and classes.

The class system itself is implemented by the racket/class library, and the racket/gui/base library provides the GUI and drawing classes. By convention, the classes are given names that end with %:

    (require racket/class
             racket/gui/base)
    (define f (new frame% [label "My Art"]
                          [width 300]
                          [height 300]
                          [alignment '(center center)]))

     
    > (send f show #t)

The new form creates an instance of a class, where initialization arguments like label and width are provided by name. The send form calls a method of the object, such as show, with arguments after the method name; the argument #t in this case is the boolean constant “true.”

Pictures generated with slideshow encapsulate a function that uses the graphics toolbox’s drawing commands to render the picture to a drawing context, such as a canvas in a frame. The make-pict-drawer function from slideshow exposes a picture’s drawing function. We can use make-pict-drawer in a canvas-painting callback to draw a picture into a canvas:

    (define (add-drawing p)
      (let ([drawer (make-pict-drawer p)])
        (new canvas% [parent f]
                     [style '(border)]
                     [paint-callback (lambda (self dc)
                                       (drawer dc 0 0))])))

     
    > (add-drawing (pict+code (circle 10)))

    #(struct:object:canvas% ...)
    > (add-drawing (colorize (filled-flash 50 30) "yellow"))

    #(struct:object:canvas% ...)

    [image]

Each canvas stretches to fill an equal portion of the frame, because that’s how a frame manages its children by default.

**12. Where to Go From Here**

This introduction to Racket purposely avoids many of the traditional ways of introducing and distinguishing Lisp or Scheme: prefix arithmetic notation, symbols, quoting and quasiquoting lists, eval, first-class continuations, and the idea that all syntax is really just a lambda in disguise. While those are all part of Racket, they are not the main ingredients of day-to-day programming in Racket.

Instead, Racket programmers typically program with functions, records, objects, exceptions, regular expressions, modules, and threads. That is, instead of a “minimalist” language—which is the way that Scheme is often described—Racket offers a rich language with an extensive set of libraries and tools.

If you are new to programming or if you have the patience to work through a textbook, we recommend reading How to Design Programs. If you have already read it, or if you want to see where the book will take you, then see Continue: Web Applications in Racket.

For experienced programmers, to continue touring Racket from a systems-oriented perspective instead of pictures, your next stop is More: Systems Programming with Racket.

To instead start learning about the full Racket language and tools in depth, move on to The Racket Guide.
