midymidytgbot
=============

MidyMidy社区的bot，用于在IRC频道和Telegram群之间传话。

目前做得差不多了，正在运行。放在这里，供想要写Bot的人参考。

其中依赖的CL-JSON魔改过，因为原版有bug，已提交pull request，但是这个开发者早就不理这个项目了。

如果有需求，想要通用一点，同时用于多个频道，或是更便于配置，就提ISSUE吧，若是我闲得慌，可能会做。

使用方法
--------

* 开启你的Bot的消息访问权限，然后把它加到群里。
* 在这个程序根目录下创建一个account_tg，在里面写`bot[token]`，如：`bot123456:ABC-DEF1234ghIkl-zyx57W2v1u123ew11`
* 在bot.lisp里面，设置好你们Telegram群的ID，你Bot的ID，你们IRC的频道，Bot的IRC名字。
* 最后：`sbcl --load loader.lisp`

TODO
----

* 寻找bug。
