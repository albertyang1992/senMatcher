#########################
#####senMatcher v1.0#####
#####AUTHOR: YANGJUN#####
#########################


#####普通规则#####
senMatching_normal<-function(rule, text){
  text_temp<-text
  #规则拆解#
  rule_pattern<-str_trim(strsplit(rule, "=", fixed = T)[[1]][1])
  rule_element<-str_trim(strsplit(rule, "=", fixed = T)[[1]][2])

  rule_patterns<-str_trim(strsplit(rule_pattern, "+", fixed = T)[[1]])#句式#
  rule_elements<-str_trim(strsplit(rule_element, "+", fixed = T)[[1]])#要素#

  rule_patterns_len<-length(rule_patterns)#句式长度#
  rule_elements_len<-length(rule_elements)#要素长度#

  #普通规则匹配#
  rule_elements_num<-as.numeric(gsub("[^0-9]", "", rule_elements))#要素序号#
  rule_elements_iden<-gsub("N[0-9]*[0-9]:", "", rule_elements)#要素身份#

  # noresults_normal<<-1#<<公式中全局赋值#
  for(i in 1:length(text_temp)){
    key1<-rep(0, rule_patterns_len)#判断句式各项是否符合的向量#
    key2<-rep(0, rule_elements_len)#判断要素各项是否符合的向量#
    for(j in 1:rule_patterns_len){
      key1[j]<-length(grep(rule_patterns[j], text_temp[i+j-1]))
    }
    if(sum(key1)==rule_patterns_len){
      rule_result_table<-as.data.frame(matrix(NA, nrow=rule_elements_len, ncol=2))
      colnames(rule_result_table)<-c("iden", "text")#结果以k*2表格输出，再dcast#
      for(k in 1:rule_elements_len){
        key2[k]<-rule_elements_num[k]#要素序号可以不从第一项开始#
        rule_result_table$iden[k]<-rule_elements_iden[k]
        rule_result_table$text[k]<-text_temp[i+key2[k]-1]#要用要素序号#
        rule_result_table$form[k]<-substr(rule_result_table$text[k], 1, gregexpr("/", rule_result_table$text[k])[[1]][1]-1)
        #form为词形#
      }
      rule_result_table2<-dcast(rule_result_table, iden~., value.var = "form", fun.aggregate = paste, collapse='')
      #同iden的要素合并输出#
      colnames(rule_result_table2)[2]<-"form"
      break
    }
    else{
      rule_result_table2<-NULL
      next
    }
  }
  return(rule_result_table2)
}


#####带有越过项的规则#####
senMatching_leap<-function(rule, text){
  text_temp<-text
  ###规则拆解###
  rule_pattern<-str_trim(strsplit(rule, "=", fixed = T)[[1]][1])
  rule_element<-str_trim(strsplit(rule, "=", fixed = T)[[1]][2])

  rule_patterns<-str_trim(strsplit(rule_pattern, "+", fixed = T)[[1]])#句式#
  rule_elements<-str_trim(strsplit(rule_element, "+", fixed = T)[[1]])#要素#

  ###越过项###
  #leap_patterns_loc为空，则句式中不存在越过项#
  #没有越过项直接进普通规则#
  leap_patterns_loc<-grep("#[0-9]*[0-9]:[0-9]*[0-9]#", rule_patterns)#识别#1:20#在patterns的位置#
  #leap_patterns_loc不为空，leap_elements_loc为空，则句式中有越过项但要素中没有#
  #该情况需要对越过项建设虚拟要素#
  leap_elements_loc<-grep(paste0("N", leap_patterns_loc, ":"), rule_elements)#识别越过项在elements的位置#

  none_leap<-as.numeric(unlist(str_extract_all(rule_elements, "[0-9]*[0-9]")))
  leap_elements_dummy_loc<-which(none_leap>leap_patterns_loc)[1]-1#越过项dummy项在要素中的位置，减去1#
  if(length(leap_elements_loc)==0){
    rule_elements<-append(rule_elements, paste0("N", leap_patterns_loc, ":leap"), leap_elements_dummy_loc)
    leap_elements_loc<-leap_elements_dummy_loc+1#dummy项在要素中的位置#
  }else{
    rule_elements<-rule_elements
  }#若leap_elements_loc为空，创建dummy项#

  leap_detail<-gsub("#[0-9]*[0-9]:[0-9]*[0-9]#", "", rule_patterns[leap_patterns_loc])#越过项的正文部分#
  leap_symbol1<-gregexpr("#", rule_patterns[leap_patterns_loc])#越过项中“#”位置#
  leap_symbol2<-gregexpr(":", rule_patterns[leap_patterns_loc])#越过项中“:”位置#
  leap_start<-substr(rule_patterns[leap_patterns_loc],
                     leap_symbol1[[1]][1]+1, leap_symbol2[[1]][1]-1)#第一个“#”到“:”间的数字即越过量最小值#
  leap_end<-substr(rule_patterns[leap_patterns_loc],
                   leap_symbol2[[1]][1]+1, leap_symbol1[[1]][2]-1)#“:”到第二个“#”间的数字即越过量最大值#
  leap_start<-as.numeric(leap_start)
  leap_end<-as.numeric(leap_end)

  rule_patterns_len<-length(rule_patterns)#句式长度#
  rule_elements_len<-length(rule_elements)#要素长度#

  ###越过项规则匹配###
  for(i in 1:length(text_temp)){
    inner_loop<-1
    for(h in leap_start:leap_end){
      rule_patterns_exp<-c(rule_patterns[1:(leap_patterns_loc-1)],
                           rep(leap_detail, h),
                           rule_patterns[(leap_patterns_loc+1):rule_patterns_len])
      #句式中有越过项的话，越过项前后一定要有个开始词和终结词，所以越过项在句式中不会是第一项或最后一项#
      #扩充句式, patterns起始项到越过项位置+复制h个越过项+越过项位置到patterns结尾项#
      if(rule_elements_len==1){#判断越过项是不是唯一的要素#
        rule_elements_exp<-rep(rule_elements[leap_elements_loc], h)
      }
      else if(leap_elements_loc==1){#越过项位置是第一个#
        rule_elements_exp<-c(rep(rule_elements[leap_elements_loc], h),
                             rule_elements[(leap_elements_loc+1):rule_elements_len])
      }
      else if(leap_elements_loc<rule_elements_len){#越过项在当中#
        rule_elements_exp<-c(rule_elements[1:(leap_elements_loc-1)],
                             rep(rule_elements[leap_elements_loc], h),
                             rule_elements[(leap_elements_loc+1):rule_elements_len])
      }
      else{#越过项是最后一个#
        rule_elements_exp<-c(rule_elements[1:(leap_elements_loc-1)],
                             rep(rule_elements[leap_elements_loc], h))
      }
      rule_patterns_exp_len<-length(rule_patterns_exp)#扩充后的句式长度#
      rule_elements_exp_len<-length(rule_elements_exp)#扩充后的要素长度#

      key1<-rep(0, rule_patterns_exp_len)#判断句式各项是否符合的向量#
      for(j in 1:rule_patterns_exp_len){
        key1[j]<-length(grep(rule_patterns_exp[j], text_temp[i+j-1]))
      }

      if(sum(key1)==rule_patterns_exp_len){
        key2<-rep(0, rule_elements_exp_len)#判断要素各项是否符合的向量#
        rule_result<-NULL
        rule_elements_exp_num<-as.numeric(gsub("[^0-9]", "", rule_elements_exp))#扩充后的要素序号#
        #以c(1,2,2,2,2,2,5,6)为例，扩充处于2的越过项，最后要素序号应变为c(1,2,3,4,5,6,9,10)#
        #在越过项之前的，要素序号不变；越过项自身累加1；越过项之后的增加h-1#
        rule_elements_exp_num2<-rule_elements_exp_num[which(rule_elements_exp_num<leap_patterns_loc)]
        #num2是越过项之前的要素部分，其要素序号不变#
        rule_elements_exp_num3_index<-rule_elements_exp_num[which(rule_elements_exp_num==leap_patterns_loc)]
        rule_elements_exp_num3<-rule_elements_exp_num3_index[1]:(rule_elements_exp_num3_index[1]+h-1)
        #num3是越过项的要素部分，其要素序号需要累加1#
        rule_elements_exp_num4<-rule_elements_exp_num[which(rule_elements_exp_num>leap_patterns_loc)]+h-1
        #num4是越过项之后的要素部分，其要素序号需要增加h-1#
        rule_elements_exp_num<-c(rule_elements_exp_num2, rule_elements_exp_num3, rule_elements_exp_num4)
        #num2, num3, num4依次合并#
        rule_elements_exp_iden<-gsub("N[0-9]*[0-9]:", "", rule_elements_exp)#要素身份#

        rule_result_table<-as.data.frame(matrix(NA, nrow=rule_elements_exp_len, ncol=2))
        colnames(rule_result_table)<-c("iden", "text")#结果以k*2表格输出，再dcast#
        for(k in 1:rule_elements_exp_len){
          key2[k]<-rule_elements_exp_num[k]#要素序号可以不从第一项开始#
          rule_result_table$iden[k]<-rule_elements_exp_iden[k]
          rule_result_table$text[k]<-text_temp[i+key2[k]-1]#要用要素序号#
          rule_result_table$form[k]<-substr(rule_result_table$text[k], 1, gregexpr("/", rule_result_table$text[k])[[1]][1]-1)
          #form为词形#
        }
        rule_result_table2<-dcast(rule_result_table, iden~., value.var = "form", fun.aggregate = paste, collapse='')

        #同iden的要素合并输出#
        colnames(rule_result_table2)[2]<-"form"
        inner_loop<-0#inner loop结束的信号#

        break
      }
      else{
        rule_result_table2<-NULL
        next
      }
    }
    if(inner_loop==0){
      break#inner loop结束，则outer loop也结束#
    }
    else next
  }
  return(rule_result_table2)
}


#####可选项保留公式opt1#####
senMatching_opt1<-function(rule, text){
  text_temp<-text
  ###规则拆解###
  rule_pattern<-str_trim(strsplit(rule, "=", fixed = T)[[1]][1])
  rule_element<-str_trim(strsplit(rule, "=", fixed = T)[[1]][2])

  rule_patterns<-str_trim(strsplit(rule_pattern, "+", fixed = T)[[1]])#句式#
  rule_elements<-str_trim(strsplit(rule_element, "+", fixed = T)[[1]])#要素#

  rule_patterns_len<-length(rule_patterns)#句式长度#
  rule_elements_len<-length(rule_elements)#要素长度#

  ###可选项###
  optional_patterns_loc<-grep("<.+/.+>", rule_patterns)
  optional_elements_loc<-grep(paste0("N", optional_patterns_loc, ":"), rule_elements)#识别可选项在elements的位置#


  ###带可选opt1###
  rule_patterns_opt1<-gsub("<|>", "", rule_patterns)
  rule_elements_opt1<-rule_elements#带可选项时，要素不变#

  rule_patterns_opt1_len<-length(rule_patterns_opt1)#opt1句式长度#
  rule_elements_opt1_len<-length(rule_elements)#opt1要素长度#
  rule_elements_opt1_num<-as.numeric(gsub("[^0-9]", "", rule_elements_opt1))#opt1的要素序号#


  for(i in 1:length(text_temp)){
    key1<-rep(0, rule_patterns_opt1_len)#判断句式各项是否符合的向量#
    key2<-rep(0, rule_elements_opt1_len)#判断要素各项是否符合的向量#
    for(j in 1:rule_patterns_opt1_len){
      key1[j]<-length(grep(rule_patterns_opt1[j], text_temp[i+j-1]))
    }
    if(sum(key1)==rule_patterns_opt1_len){
      rule_elements_opt1_iden<-gsub("N[0-9]*[0-9]:", "", rule_elements_opt1)#要素身份#

      rule_result_table<-as.data.frame(matrix(NA, nrow=rule_elements_opt1_len, ncol=2))
      colnames(rule_result_table)<-c("iden", "text")#结果以k*2表格输出，再dcast#
      for(k in 1:rule_elements_opt1_len){
        key2[k]<-rule_elements_opt1_num[k]#要素序号可以不从第一项开始#
        rule_result_table$iden[k]<-rule_elements_opt1_iden[k]
        rule_result_table$text[k]<-text_temp[i+key2[k]-1]#要用要素序号#
        rule_result_table$form[k]<-substr(rule_result_table$text[k], 1, gregexpr("/", rule_result_table$text[k])[[1]][1]-1)
        #form为词形#
      }
      rule_result_table2<-dcast(rule_result_table, iden~., value.var = "form", fun.aggregate = paste, collapse='')

      #同iden的要素合并输出#
      colnames(rule_result_table2)[2]<-"form"

      break
    }
    else{
      rule_result_table2<-NULL
      next
    }
  }
  return(rule_result_table2)
}


#####可选项删除公式opt2#####
senMatching_opt2<-function(rule, text){
  text_temp<-text
  ###规则拆解###
  rule_pattern<-str_trim(strsplit(rule, "=", fixed = T)[[1]][1])
  rule_element<-str_trim(strsplit(rule, "=", fixed = T)[[1]][2])

  rule_patterns<-str_trim(strsplit(rule_pattern, "+", fixed = T)[[1]])#句式#
  rule_elements<-str_trim(strsplit(rule_element, "+", fixed = T)[[1]])#要素#

  rule_patterns_len<-length(rule_patterns)#句式长度#
  rule_elements_len<-length(rule_elements)#要素长度#

  ###可选项###
  optional_patterns_loc<-grep("<.+/.+>", rule_patterns)
  optional_elements_loc<-grep(paste0("N", optional_patterns_loc, ":"), rule_elements)#识别可选项在elements的位置

  ###不带可选opt2###
  rule_patterns_opt2<-rule_patterns[-optional_patterns_loc]
  #比如elements_opt2_num是1234，可选项在2，需变成123，即去掉2，34变23#
  #如果要素里没有可选项，则optional_elements_loc为空，此处rule_elements_opt2不变#
  if(length(optional_elements_loc)==0){
    rule_elements_opt2<-rule_elements
  }else{
    rule_elements_opt2<-rule_elements[-optional_elements_loc]
  }
  rule_elements_opt2_num<-as.numeric(gsub("[^0-9]", "", rule_elements))#删除可选项前的要素序号#
  #opt2_num1代表要素向量中可选项之前的要素#
  #可选项前面的，保持不变#
  rule_elements_opt2_num1<-rule_elements_opt2_num[which(rule_elements_opt2_num<optional_patterns_loc)]
  #可选项后面的，减去1，这里只支持一个可选项！！！#
  rule_elememts_opt2_num2<-rule_elements_opt2_num[which(rule_elements_opt2_num>optional_patterns_loc)]-1
  rule_elements_opt2_num<-c(rule_elements_opt2_num1, rule_elememts_opt2_num2)#删除可选项后的要素序号#

  rule_patterns_opt2_len<-length(rule_patterns_opt2)#opt2句式长度#
  rule_elements_opt2_len<-length(rule_elements_opt2)#opt2要素长度#

  for(i in 1:length(text_temp)){
    key1<-rep(0, rule_patterns_opt2_len)#判断句式各项是否符合的向量#
    key2<-rep(0, rule_elements_opt2_len)#判断要素各项是否符合的向量#
    for(j in 1:rule_patterns_opt2_len){
      key1[j]<-length(grep(rule_patterns_opt2[j], text_temp[i+j-1]))
    }
    if(sum(key1)==rule_patterns_opt2_len){
      rule_elements_opt2_iden<-gsub("N[0-9]*[0-9]:", "", rule_elements_opt2)#删除可选项后的要素身份#

      rule_result_table<-as.data.frame(matrix(NA, nrow=rule_elements_opt2_len, ncol=2))
      colnames(rule_result_table)<-c("iden", "text")#结果以k*2表格输出，再dcast#
      for(k in 1:rule_elements_opt2_len){
        key2[k]<-rule_elements_opt2_num[k]#要素序号可以不从第一项开始#
        rule_result_table$iden[k]<-rule_elements_opt2_iden[k]
        rule_result_table$text[k]<-text_temp[i+key2[k]-1]#要用要素序号#
        rule_result_table$form[k]<-substr(rule_result_table$text[k], 1, gregexpr("/", rule_result_table$text[k])[[1]][1]-1)
        #form为词形#
      }
      rule_result_table2<-dcast(rule_result_table, iden~., value.var = "form", fun.aggregate = paste, collapse='')

      #同iden的要素合并输出#
      colnames(rule_result_table2)[2]<-"form"

      break
    }
    else{
      rule_result_table2<-NULL
      next
    }
  }
  return(rule_result_table2)
}


#####通用算法general#####
#优先度：可选项>越过项>普通规则#
#句式中，可选项和越过项不能同时出现#
#句式中，可选项和越过项也只能出现一次#
#越过项是就短匹配的#
#senMatching_single是基本公式，针对单规则单文本，规则与文本是char#
#senMatching_general是多规则多文本，规则与文本是char vector#

#单规则单文本匹配#
senMatching_single<-function(rule, text){
  rule<-rule
  text<-text
  #规则拆解#
  rule_pattern<-str_trim(strsplit(rule, "=", fixed = T)[[1]][1])
  rule_element<-str_trim(strsplit(rule, "=", fixed = T)[[1]][2])
  rule_patterns<-str_trim(strsplit(rule_pattern, "+", fixed = T)[[1]])#句式#
  rule_elements<-str_trim(strsplit(rule_element, "+", fixed = T)[[1]])#要素#

  #判断句式类型#
  optional_patterns_loc<-grep("<.+/.+>", rule_patterns)#可选项位置#
  leap_patterns_loc<-grep("#[0-9]*[0-9]:[0-9]*[0-9]#", rule_patterns)#越过项位置#

  #逻辑判断#
  if(length(optional_patterns_loc)>0){
    rule_result_table2<-senMatching_opt1(rule, text)
    rule_result_table2<-senMatching_opt2(rule, text)
  }else if(length(leap_patterns_loc)>0){
    rule_result_table2<-senMatching_leap(rule, text)
  }else{
    rule_result_table2<-senMatching_normal(rule, text)
    # if(noresults_normal!=0){
    #   # cat("No Results!")
    #   noresults_single<<-1#noresults_general为1代表无法匹配上#
    # }
  }
  return(rule_result_table2)
}

#多规则多文本匹配#
senMatching_general<-function(rule_list, text_list, tokenizer="jieba"){
  #选择分词器#
  if(tokenizer!="cnc"){
    #如果不用cnc分词，都默认用jieba#
    #jieba分词与词性标注#
    mixseg<-worker()
    tagseg<-worker('tag')
    tagseg_x<-function(x){
      a<-names(tagseg[x])#词性
      b<-tagseg[x]#词
      c<-paste(b,a,sep = "/",collapse = "-")
      return(c)
    }

    jieba_list<-rep("", length(text_list))
    for(k in 1:length(text_list)){
      jieba_list[k]<-tagseg_x(text_list[k])
    }

    jieba_list2<-strsplit(jieba_list,"-")#根据-来拆分#

    taglist<-jieba_list2

  }else{
    #同时支持cnc分词，不过不对外公开#
    #利用reticulate运行python指令#
    #运行seg.py脚本#
    source_python("seg.py")
    seg<-Seg()

    cncseg_x<-function(x){
      a<-seg$cut(x)
      a<-str_replace_all(a, "  ", " ")
      b<-strsplit(a, " ")[[1]]
      return(b)
    }

    for(i in 1:length(text_list)){
      cnc_list[i]<-seg$tolist(seg$cut(text_list[i]))
    }
    taglist<-cnc_list
  }

  #rule_result_table3#
  rule_result_table3<-NULL
  for(i in 1:length(taglist)){
    for(j in 1:length(rule_list)){
      rule_result_table2<-senMatching_single(rule_list[j], taglist[[i]])
      if(!is.null(rule_result_table2)){
        rule_result_table2$textid<-i
        rule_result_table2$text<-text_list[i]
        rule_result_table2$rule<-rule_list[j]
        rule_result_table3<-rbind(rule_result_table3, rule_result_table2)#最终输出#
      }
      else next
    }
    #显示进度#
    if(i %% round(length(text_list)/20) ==0){
      cat("text coverage is ", i/length(text_list)*100, "%", "\n", sep = "")
    }
  }
  return(rule_result_table3)
}

#senMatching_general(rule_list = rule_list, text_list = text_list)
##








