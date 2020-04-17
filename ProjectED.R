#' ---
#' title: "Projekt.Eksploracja danych. Zastosowanie przecinania drzew klasyfikacyjnych i selekcji atrybutów istotnych dla zwiększenia wydajności klasyfikatora drzew decyzijnych "
#' author: "Darya Khordykova.w57033"
#' date: "26 12 2019"
#' output:
#'   pdf_document:
#'     latex_engine: xelatex
#' ---
## ------------------------------------------------------------------------
library(caTools)
library(randomForest)
library(gbm)
library(rpart)
library(caret)
library(ggplot2)
library(scales)
library(corrplot)
library(RColorBrewer)
library(dplyr)
library(partykit)
g <- "C:/Users/Darya/Documents"
if (getwd()!= g )
setwd(g) ;
getwd()
liczba_walidacji=30
lista_baz<−read.csv ( "bazki.csv" , sep=";")
ilosc_baz<−nrow(lista_baz)

podsumowenia_wynikow<−matrix(nrow=ilosc_baz, ncol=5)
colnames(podsumowenia_wynikow)<-c("Dataset name ","Base accuracy","Preprune accuracy","Postprune accuracy","Attribute selction")

wyniki_badan1<−matrix(data=NA, nrow=liczba_walidacji +1, ncol=ilosc_baz*2)
wyniki_badan2<−matrix(data=NA, nrow=liczba_walidacji +1, ncol=ilosc_baz*2)



for ( i in 1 :ilosc_baz )
{

nazwa_bazy<−lista_baz[i,2]

 print(paste("==========  Wstępna analiza danych bazy: ==========", nazwa_bazy))
  dataset<-read.csv(toString(nazwa_bazy))
 
  print(paste("==========  Summary: ==========", nazwa_bazy))
  print(summary(dataset));
  print('**********************************************************')
  print('***********************************************************')
  print('**********************************************************')
   print(paste("==========  Structure: ==========", nazwa_bazy))
  print(str(dataset))
   print('***********************************************************')
  print('**********************************************************')
  print('***********************************************************')
  print('**********************************************************')
  print(paste("==========  Attribute data type: ==========", nazwa_bazy))
  print(sapply(dataset,class))
   print('***********************************************************')
  print('**********************************************************')
  print('***********************************************************')
  print('**********************************************************')
  print(paste("==========  First samples: ==========", nazwa_bazy))
  print(head(dataset))
  print('**********************************************************')
  print('***********************************************************')
  print('**********************************************************')
  
  print(paste("==========  Last samples: ==========", nazwa_bazy))
  print(tail(dataset))
   print('***********************************************************')
  print('**********************************************************')
  print('***********************************************************')
  print('**********************************************************')
  print(paste("==========  Dataset Dimenssions: ==========", nazwa_bazy))
  print(dim(dataset))
  print(paste("==========  Histogram Plot of  Decision Classes: ==========", nazwa_bazy))
  
  decision_name<-tail(colnames(dataset),n=1)
  
 decision_cln<- paste(dataset[,ncol(dataset)])
  plot_<-ggplot(dataset, aes(x=decision_cln, fill= decision_cln)) + geom_bar(aes(y = (..count..)/sum(..count..))) + scale_y_continuous(labels=percent)
wykres1<-plot_+ labs(title = paste("wykres atrybutu decyzji", nazwa_bazy),x=decision_name)
  print(wykres1)
  
  print(paste("==========  Desity Plot of  Decision Classes: ==========", nazwa_bazy))
  wykres2<-ggplot(dataset, aes(x=dataset[,ncol(dataset)])) + geom_density(color="darkblue", fill="lightblue") + labs(title = paste("wykres atrybutu decyzji", nazwa_bazy),x=decision_name)
  print(wykres2)
 print(paste("==========  Correlation Plot of  Dataframe: ==========", nazwa_bazy)) 
  dataset.cor = cor(dataset)
  corrplot(dataset.cor)
  
  
#Analysing
  
  
wyniki_badan1[1,i*2−1]<−paste(nazwa_bazy,"Base accuracy")
wyniki_badan1[1,i*2]<−paste(nazwa_bazy,"Preprune accuracy")
wyniki_badan2[1,i*2−1]<−paste(nazwa_bazy,"Postprune accuracy")
wyniki_badan2[1,i*2]<−paste(nazwa_bazy,"Attribute selction")


podsumowenia_wynikow[i,1]<−ifelse(is.na(podsumowenia_wynikow[i,1]), as.character(nazwa_bazy), podsumowenia_wynikow[i,1])
dataset<-read.csv(toString(nazwa_bazy))
decision_name<-tail(colnames(dataset),n=1)
cln_nmb<-which(colnames(dataset)==decision_name)


dataset[,-cln_nmb] = scale(dataset[,-cln_nmb])

waznosc.rf<−randomForest(dataset[,ncol(dataset)]~ ., data= dataset,importance=TRUE, ntree=500) 
imp_list<-waznosc.rf$importance
imp_list<-imp_list[-nrow(imp_list),2]

atrybuty_istotne<−names(which(imp_list>mean(imp_list)))

plot( waznosc.rf, main = paste( "Baza",nazwa_bazy))
varImpPlot(waznosc.rf, main = paste ( "Baza", nazwa_bazy))
indexes <− sample(1:dim(dataset)[1],nrow(dataset))
dataset <− dataset[indexes,]
training_indexes <− createFolds(dataset[,cln_nmb], k=liczba_walidacji)
for ( j in 1 : liczba_walidacji ){
 
training_set<−dataset[unlist((training_indexes[j])),]
test_set<-dataset[−unlist(training_indexes[j]),]  
  
decision_name<-tail(colnames(dataset),n=1)
cln_nmb<-which(colnames(dataset)==decision_name)
formula_general<-as.formula(paste(decision_name, "~."))



#Base Decision tree 
base_tree<- rpart(formula=formula_general, data =training_set,method ="class")



#Base Decision tree  prediction
predict(base_tree, newdata=test_set ,type="class")->pred_base_tr

#Preprune decision tree
hr_model_preprun<-rpart(formula_general, data = training_set , method = "class", control = rpart.control(cp = 0,minbucket = round(1/ 3), minsplit = 50, maxdepth = 15))
#Preprune decision tree prediction
predict( hr_model_preprun , newdata=test_set, type="class")−>pred_preprun_tr
#Postpruning decision tree
cp.optim <- base_tree$cptable[which.min(base_tree$cptable[,"xerror"]),"CP"]
hr_model_pruned <- prune(base_tree, cp = cp.optim,type="class")
#Postpruning decision tree prediction
predict( hr_model_pruned , newdata=test_set, type="class")−>pred_postprun_tr 
#formula creation
formula<-as.formula(paste(decision_name, paste(atrybuty_istotne, collapse=" + "), sep=" ~ "))
print(formula)
#Selected attribute model
model_attr<−rpart(formula,data=training_set,method = "class")
#Selected attribute model predict
predict(model_attr, newdata=test_set[atrybuty_istotne],type="class")−>pred_select_attr 


#Prediction matrix Base Decision tree 
base_tr_md<−table(test_set[,cln_nmb], pred_base_tr )
print("Prediction matrix Base Decision tree ")
print(base_tr_md)
#Prediction matrix Preprune decision tree
preprun_tr_md<−table(test_set[,cln_nmb], pred_preprun_tr )
print("Prediction matrix Preprune decision tree")
print(preprun_tr_md)
#Prediction matrix Postpruning decision tree
postprun_tr_md<−table(test_set[,cln_nmb], pred_postprun_tr )
print("Prediction matrix Postpruning decision tree")
print(postprun_tr_md)
#Prediction matrix Selected attribute model
selected_tr_md<−table(test_set[,cln_nmb], pred_select_attr )
print("Prediction matrix Selected attribute model")
print(selected_tr_md)
#Model error
err_base_tr<−(sum( diag (base_tr_md ) ) /sum(base_tr_md ) )
err_preprun_tr<−(sum( diag (preprun_tr_md ) ) /sum(preprun_tr_md ) )
err_postprun_tr<−(sum( diag (postprun_tr_md ) ) /sum(postprun_tr_md ) )
err_selected_tr<−(sum( diag (selected_tr_md ) ) /sum(selected_tr_md ) )

wyniki_badan1[j+1,i*2−1]<−round(err_base_tr,3)
wyniki_badan1[j+1,i*2]<−round(err_preprun_tr,3)
wyniki_badan2[j+1,i*2−1]<−round(err_postprun_tr,3)
wyniki_badan2[j+1,i*2]<−round(err_selected_tr,3)

}


podsumowenia_wynikow[i,2] <−paste(round(mean(as.numeric(wyniki_badan1[2:j, i*2−1]))* 100, 2 ), "%")
podsumowenia_wynikow[i,3] <−paste(round(mean(as.numeric(wyniki_badan1[2:j, i*2]))* 100, 2 ), "%")
podsumowenia_wynikow[i,4] <−paste(round(mean(as.numeric(wyniki_badan2[2:j, i*2−1]))* 100, 2 ), "%")
podsumowenia_wynikow[i,5] <−paste(round(mean(as.numeric(wyniki_badan2[2:j, i*2]))* 100, 2 ), "%")

}

print(cbind(wyniki_badan1, wyniki_badan2))
print(podsumowenia_wynikow)

#' 
#' #Analiza wyników oraz wnioski
#' W ramach danego projektu byli  zastosowane następne metody zwiększenia wydajności działania klasyfikatora drzew decyzyjnych: algorytmy przecinania drzewa przez funkcję „repart”,  selekcja atrybutów .Dla sprawdzenia działalności danych metod było zastosowane podejście obliczenia błędu klasyfikacji. Dla analizy były wykorzystane  4 zbiory danych : Adult.csv, Bank.csv,Abalone.csv, Mashroom.csv. Otrzymane wyniki udowodniają zwiększenie wydajności klasyfikatora  przez metody przecinania drzewa decyzji .  Pierwsza metoda obcinania drzewa   polega na wprowadzeniu do funkcji rpart parametru kontroli rozszerzenia drzewa decyzji . Aby poprawić działalność klasyfikatora możemy na-rzucić ograniczenia na rozbudowę drzewa za pomocą parametru kontroli poprzez określenie minimalnej liczby elementów w liściu za pomocą parametru minsplit  , maksymalnej wysokości drzewa przez parametr maxdepth , minimalnej ilości obserwacji w „liściach” drzewa za pomocą parametru minbucket. Inną metodą  przecinania drzewa jest dopasowanie wskaźnika złożoności  cp.  W porównaniu do metod obcięcia drzewa decyzji, metoda   selekcji atrybutów istotnych działa gorzej. 
#' 
#' 
#' 
#' 
#' 
#' 
