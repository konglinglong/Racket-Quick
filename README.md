
# Racket快速浏览
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

也就是说，标识符circle被绑定到一个函数，就像c被绑定到一个圆一样。与圆形图不同，没有一种简单的方法可以完全打印函数，所以DrRacket只打印#<procedure:circle>。
这个例子显示了函数是值，就像数字和图片一样(即使它们不能很好地打印)。因为函数是值，你可以将函数作为其他函数的参数:


```
(define (series mk)
      (hc-append 4 (mk 5) (mk 10) (mk 20)))
     
> (series circle)
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_11.png)

```
> (series square)
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_12.png)

当一个函数仅仅作为参数，而其他地方没有用到时，还要通过define将函数写下来是一件麻烦事，因为你必须创建一个名称并找到放置函数定义的位置。另一种更好的选择是使用lambda，它创建了一个匿名函数:

```
> (series (lambda (size) (checkerboard (square size))))
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_13.png)

lambda后面括号内是函数的参数，紧接后面的表达式是函数体。用“lambda”代替“功能”或“过程”是Racket历史和文化的一部分。
函数的定义形式实际上是使用lambda作为值的简单定义的简写。例如，series定义可以写成

```
(define series
      (lambda (mk)
        (hc-append 4 (mk 5) (mk 10) (mk 20))))
```

大多Racket程序员更喜欢使用带有define的简写函数形式，而不是扩展到lambda。

**7. 词法作用域**

Racket是一种词法限定的语言，这意味着无论何时将标识符用作表达式，表达式的文本环境中的某些东西都会决定该标识符的绑定。此规则适用于lambda主体中的标识符，也适用于其他任何地方。
在下面的rgb-series函数中，mk在每个lambda形式中的使用都引用了rgb-series的参数，因为这是在作用域中的文本绑定:

```
(define (rgb-series mk)
      (vc-append
       (series (lambda (sz) (colorize (mk sz) "red")))
       (series (lambda (sz) (colorize (mk sz) "green")))
       (series (lambda (sz) (colorize (mk sz) "blue")))))
     
> (rgb-series circle)
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_14.png)

```
> (rgb-series square)
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_15.png)

这里是另一个例子，其中rgb-maker获取一个函数并返回一个新的函数，该函数记住并使用原始函数。

```
(define (rgb-maker mk)
      (lambda (sz)
        (vc-append (colorize (mk sz) "red")
                   (colorize (mk sz) "green")
                   (colorize (mk sz) "blue"))))
     
> (series (rgb-maker circle))
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_16.png)

```
    > (series (rgb-maker square))
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_17.png)

请注意，与使用rgb-series相比，通过rgb-maker组合函数如何在图片中创建不同的对象对齐方式。

**8. Lists**

Racket很大程度上继承了Lisp语言的风格，Lisp一词最初的意思是“列表处理器”，而列表仍然是Racket的重要组成部分。
list函数接受任意数量的参数，并返回一个包含给定值的列表:

```
> (list "red" "green" "blue")
'("red" "green" "blue")
> (list (circle 10) (square 10))
```
'(![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_18.png) ![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_19.png))

如您所见，列表打印为单引号跟一对圆括号，括号里面是列表元素。这里很容易混淆，因为括号同时用于表达式，如(circle10)和打印结果，如'("red" "green" "blue")。唯一的区别是列表打印前面有个单引号。
如果您有一个列表，那么您最终会希望对每个元素进行处理。map函数接受一个列表和一个应用于列表的每个元素的函数，它返回一个新的列表，以组合函数的结果:

```
(define (rainbow p)
      (map (lambda (color)
             (colorize p color))
           (list "red" "orange" "yellow" "green" "blue" "purple")))
     
> (rainbow (square 5))
```
'(![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_20.png) ![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_21.png) ![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_22.png) ![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_23.png) ![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_24.png) ![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_25.png))

另一个处理列表的函数是apply。像map一样，它接受一个函数和一个列表，但是一个指定应用的函数应该一次接受所有的参数，而不是一个一个的。apply函数对于接受任意数量参数的函数特别有用，比如vc-append:

```
> (apply vc-append (rainbow (square 5)))
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_26.png)

注意，(vc-append (rainbow (square 5))不能工作，因为vc-append不需要一个列表作为参数;它想要一张图片作为参数，而且它愿意接受任何数量的图片。apply函数将需要多个参数的函数和作为单个值的参数列表之间的空隙连接起来。

**9. Modules**

在你的定义窗口，程序是这么开头的

```
#lang slideshow
```

定义窗口的所有代码都属于一个模块。这个模块会在初始时导入slideshow模块，比如图片生成函数以及其他常用函数，如list和map。
要导入外部库，需要使用require语句。例如，库pict/flash提供了一个填充flash函数:

```
(require pict/flash)
     
> (filled-flash 40 30)
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/pict_27.png)


模块有多种全名及发布方式:
    
- 有些模块是Racket发行版自带，安装在collects目录下。例如，模块名pict/flash表示此模块是在pict文件夹下的"flash.rkt"中实现。当一个模块名不包含斜杠时，它指的是"main.rkt"文件。

    
- 一些模块作为包分发。可以通过DrRacket菜单中的安装选项进行安装，或者通过raco pkg命令安装。

    
- 一些模块依赖于其他模块，但不是必须属于何特定的集合或包。例如，在DrRacket中，如果你将你的定义保存到文件“quick.rkt”中。然后加上这一行

```
(provide rainbow square)
```

然后你可以在DrRacket中新建程序“use.rkt”，并与"quick.rkt"在同一目录下:

```
#lang racket
(require "quick.rkt")
(rainbow (square 5))
```

当你运行“use.rkt”，就会输出一个彩虹方块。注意,“use.rkt”仅仅导入了racket，而racket本身不提供任何图像生成函数，而是提供了require和函数调用语法。

Racket程序员通常将程序和库写成模块，然后通达相对路径相互引用。这种开发方式还可以将其注册为包，方便其他人使用。

**10. Macros**

这是另外一个库:

```
(require slideshow/code)
     
> (code (circle 10))
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/img0.png)

其结果不是圆，而是代码的图片，如果将其用作表达式，则会生成一个圆。换句话说，code不是一个函数，而是一种生成图片的语法;括住code的括号中间的东西不是表达式，而是code的语法形式。

这就解释了我们在上一节中提到的racket提供了require和函数调用语法的含义。库不限于包含函数，还可以定义新的语法形式。从这个意义上说，racket不能算是一种语言，而是一种可以创造语言的语言，可以非常方便的扩展或创建全新的语言。

一种创造新语法形式的方法是通过define-syntax和syntax-rules:

```
(define-syntax pict+code
      (syntax-rules ()
        [(pict+code expr)
         (hc-append 10
                    expr
                    (code expr))]))
     
> (pict+code (circle 10))
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/img1.png)

这种是一个宏定义。(pict+code expr)部分代表宏的匹配模式，在程序中匹配上的部分会被替换成相应的模板，即(hc-append 10 expr (code expr))。具体来说，(pict+code (circle 10))中的(circle 10)将与expr匹配，整个表达式替换为(hc-append 10 (circle 10) (code (circle 10))。

当然，这种语法扩展是一把双刃剑：它让你随心所欲的发明一种新的语言的同时，也让别人更难理解。

实际上，如果你查看一下这个文档的源代码。你会看到它除了以#lang开头，其他地方看起来不太像Racket。尽管如此，这份文档的确是用Racket撰写的。为了撰写这份文档，我们使用了大量的syntax-rules来扩展语法

**11. 对象**

对象系统是另一个复杂的语言扩展的例子，值得Racket用户学习和使用。对象有时比函数更好，即使您使用lambda，而且对象对于图形用户界面更为有效。Racket的用户界面接口和图形系统就是用类和对象实现的。

类系统本身由racket/class库实现，racket/gui/base库提供图形用户界面和绘图类。这些类习惯以%结尾:

```
(require racket/class
         racket/gui/base)
    (define f (new frame% [label "My Art"]
                          [width 300]
                          [height 300]
                          [alignment '(center center)]))
     
> (send f show #t)
```

new语句创建一个类的实例，并对label和width等参数进行初始化。send语句调用对象的一个方法，例如show，方法名后面跟着参数，本例中的参数#t是布尔常量“true”。

就像把一副画布嵌入到画框中，图形化工具箱可以把图片呈现到绘图上下文中。我们可以使用slideshow中的make-pict-drawer函数以回调的方式把一副图片嵌入到画布中:

```
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
```
![image](https://github.com/konglinglong/Racket-Quick/blob/master/images/img2.png)

每个画布都把画框填满，这是画框对组件默认的处理方式。

**12. 接下来**

这篇教程刻意回避了许多Lisp和Racket的经典内容：前缀表达式、符号、引用、quasiquoting lists, eval, first-class continuations，所有这些语法实际都是lambda的变形。虽然这些都是Racket的一部分，但它们并不是Racket编程经常用到的知识。

相反，Racket程序员通常使用函数、记录、对象、异常、正则表达式、模块和线程进行编程。也就是说，Racket并不志在做一个“极简”语言，而是通过大量的扩展库和工具提供丰富的支持。

如果你是编程新手，或者你只有书本的知识，我们推荐你阅读《How to Design Programs》。如果你已经读过这本书，或者你想知道这本书会能干啥，可以去看看《Continue: Web Applications in Racket》。

对于有经验的程序员，想要从系统的角度来继续学习Racket，你的下一站应该是《More: Systems Programming with Racket.》

想要对Racket语言和工具有更全面的了解，可以去看《The Racket Guide》。
