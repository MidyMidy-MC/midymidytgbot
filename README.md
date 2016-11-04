midymidytgbot
=============

MidyMidy社区的bot，用于在IRC频道和Telegram群之间传话。

目前做了一多半。供想要写Bot的人参考。

其中依赖的CL-JSON魔改过，因为原版有bug，已提交pull request，但是这个开发者早就不理这个项目了。

TODO
----

* Telegram消息多行拆成多条消息发送
* 修改错误字符串的函数需要故障恢复功能
* 需要多方面地防备Telegram抽疯
* 将Telegram的图片通过图床传送给IRC
* Telegram回复IRC
