midymidytgbot
=============

MidyMidy社区的bot，用于在IRC频道和Telegram群之间传话。

目前做得差不多了，正在运行。放在这里，供想要写Bot的人参考。

其中依赖的CL-JSON魔改过，因为原版有bug，已提交pull request，但是这个开发者早就不理这个项目了。

如果有需求，想要通用一点，同时用于多个频道，或是更便于配置，就提ISSUE吧，若是我闲得慌，可能会做。

使用方法：

`sbcl --load loader.lisp`

TODO
----

* 寻找bug。
