# senMatcher

elements extraction for Chinese

中文要素抽取算法

作者：Albert Yang Jun

## 安装方法
install_github("albertyang1992/senMatcher")

library(senMatcher)

依赖包：stringr, jiebaR, jiebaRD

## 核心用法
output<-senMatcher_general(rule_list, text_list, tokenizer="jieba")

rule_list以char vector形式输入规则，text_list以char vector形式输入原文，分词包默认是结巴分词，开源版不支持其他分词包


## 使用方法
自己写相应句法规则，抽取对应要素

例如：200元零钱被盗

可被分词为：

>>200/m + 元/n + 零钱/n + 被盗/v

若要抽取行为和客体，规则可写为：

>>./m + ^元/. + ./n + ./v = N1:obj + N2:obj + N3:obj + N4:action

则最后返回：

>>obj:200元零钱

>>action:被盗

"="左边是句式，右边是要素

句式中"./m"是一个基本单元，以"/"分隔的左边是词型，右边是词性

词型和词性都支持正则表达式，用"."表示通配符

例如：[^电瓶车|电动车]/n|vn|np|car + ^(被偷|被盗)/. = N1:obj + N2:action


## 高级用法

### 越过项

越过项中，可越过任何不违反指明的词型和词性

原文：
>>其车内的200元零钱被盗

分词及词性标注后：

>>其车/x + 内/n + 的/uj + 200/m + 元/n + 零钱/n + 被盗/v

越过项先要锚定一头一尾，再确定越过长度，例如我们这里要抽取“其车内的”和“被盗”之间的任何词，作为被盗物品要素，规则可写为：
>>^内/. + ./uj + #1:5#[^被盗]/. + ^被盗/. = N3:obj + N4:action

此处将"./uj"和"被盗/."作为头尾，#1:5#表示可越过1-5个词

注意越过项中要写明不能包含尾部，即[^被盗]/.部分，这是为了不越过“被盗”一词，这就自然将其做成了尾部

最后也返回：

>>obj:200元零钱

>>action:被盗

^内/. + ./uj + #1:5#[^零钱]/. + ^零钱/. + ^被盗/. = N1:aaa + N2:aaa + N4:obj + N5:action

则返回：

>>aaa:内的

>>action:被盗

>>leap:200元

>>obj:零钱

这里越过项没成为要素的话，会自动产生一个leap要素代表越过项

### 可选项
可选项用<>表示，<>内写法与普通单元无异，表明这个单元可有可无

原文：
>>其车内的200元零钱被盗

分词及词性标注后：

>>其车/x + 内/n + 的/uj + 200/m + 元/n + 零钱/n + 被盗/v

./m + ^元/. + <^零钱/.> + ^被盗/. = N1:number + N3:obj + N4:action

返回为：

>>number:200

>>obj:零钱

>>action:被盗

./m + ^元/. + ^零钱/. + <^整/.> + ^被盗/. = N1:number + N3:obj + N5:action

返回为：

>>number:200

>>obj:零钱

>>action:被盗

## 使用限制

优先度：可选项>越过项>普通规则

句式中，可选项和越过项不能同时出现

句式中，可选项和越过项也只能出现一次

越过项是就短匹配的

senMatching_single是基本公式，针对单规则单文本，规则与文本是char

senMatching_general是多规则多文本，规则与文本是char vector

## 问题可咨询
yangjun@idataway.com（工作）
363579849@qq.com（个人）


