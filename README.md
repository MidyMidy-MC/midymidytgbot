midymidytgbot
=============

MidyMidy社区的bot，用于在IRC频道和Telegram群之间传话。

目前做得差不多了，正在运行。放在这里，供想要写Bot的人参考。

其中依赖的CL-JSON魔改过，因为原版有bug，已提交pull request，但是这个开发者早就不理这个项目了。

如果有需求，就提ISSUE吧，若是我闲得慌，可能会做。**master**分支不保证能运行（就是一个草稿一样的开发分支），**stable**分支是我们实际使用的分支。

使用方法
--------

* 进入文件夹`c`，执行`cmake CMakeLists.txt && make`，退回上一级文件夹（现在依赖libcurl）
* 下载一个Steel Bank Common Lisp，(Debian: `sudo apt install sbcl`)
* 开启你的Bot的消息访问权限，然后把它加到群里。
* 观察`start_example.lisp`文件，创建你自己的启动配置文件
* 在irc那一栏中填入你bot的在IRC上的名字，和要加入的IRC频道（只能填一个）
* 在tg那一栏中，填入你Telegram Bot的ID，和`bot<token>`，如：`bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11`，以及群号（是一个负数）
* 在项目目录执行命令：`sbcl --load start_example.lisp`
* 支持多个例程，需要你创建多个“bot”
* **注意**：如果Telegram群由普通群转换为超级群（似乎超级群的群号更长，像是某种掩码），群的id会变化，于是需要重新设置

TODO
----

* 寻找bug。
