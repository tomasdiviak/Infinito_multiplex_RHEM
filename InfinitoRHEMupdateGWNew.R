######## RHEM analysis of Infinito multiplex mafia network - update ########

# packages
library(survival)

# working directory with the updated data
setwd("C:/Users/msraqtd4/OneDrive - The University of 
      Manchester/práce/postdoc/DualityEventsActors")


#### meeting network as the dependent variable
events.meetings <- read.csv("InfinitoEvents_Revise2_MEETINGS.csv")

sum(events.meetings$IS_OBSERVED) # 526 meeting events /// R1 526, R2 526
summary(events.meetings$size) # min 2, med 2, mean 3.272, max 25/// R1&R2 same
hist(events.meetings$size, breaks = c(1:25)) # size histogram
summary(events.meetings) # summary stats of all instances
summary(events.meetings[events.meetings$IS_OBSERVED == 1,]) # summary of events
summary(events.meetings[events.meetings$IS_OBSERVED == 0,]) # summary of non-events

names(events.meetings)
# square root transformation of all endogenous effects (network effects based on prior events)
events.meetings[,c(22:ncol(events.meetings))] <- sqrt(events.meetings[,c(22:ncol(events.meetings))])

# strata are identified by TIME & SIZE (simultaneous events on the same day)
events.meetings$stratum <- paste(events.meetings$TIME,":", events.meetings$size)

meetings1 <- coxph(Surv(time = rep(1,nrow(events.meetings)), 
                             event = events.meetings$IS_OBSERVED) ~
                          ratio.leader
                        + diff.leader
                        + avg.age
                        + diff.age
                        + diff.locale
                        + kin
                        + meeting.exact.repetition 
                        + meeting.sub.rep.GW.5
                        + meeting.closure
                        + meeting.degree
                        + call.exact.repetition
                        + call.sub.rep.GW.5
                        + call.closure
                        + call.degree
                        + offense.exact.repetition
                        + offense.sub.rep.GW.5
                        + offense.closure
                        + offense.degree
                        + strata(stratum)
                        , robust = TRUE
                        , data = events.meetings
                        , control = coxph.control(iter.max = 40)
)
summary(meetings1)

meetings1a <- coxph(Surv(time = rep(1,nrow(events.meetings)), 
                        event = events.meetings$IS_OBSERVED) ~
                     ratio.leader
                   + diff.leader
                   + avg.age
                   + diff.age
                   + diff.locale
                   + kin
                   + meeting.exact.repetition 
                   + meeting.sub.rep.GW.5
                   + meeting.closure
                   + meeting.degree
                   + call.exact.repetition
                   + call.sub.rep.GW.5
                   + call.closure
                   #+ call.degree # NA on the estimate
                   + offense.exact.repetition
                   + offense.sub.rep.GW.5
                   + offense.closure
                   + offense.degree
                   + strata(stratum)
                   , robust = TRUE
                   , data = events.meetings
                   , control = coxph.control(iter.max = 40)
)
summary(meetings1a)

meetings2 <- coxph(Surv(time = rep(1,nrow(events.meetings)), 
                        event = events.meetings$IS_OBSERVED) ~
                     ratio.leader
                   + diff.leader
                   + avg.age
                   + diff.age
                   + diff.locale
                   + kin
                   + meeting.exact.repetition 
                   + meeting.sub.rep.GW.2
                   + meeting.closure
                   + meeting.degree
                   + call.exact.repetition
                   + call.sub.rep.GW.2
                   + call.closure
                   + call.degree
                   + offense.exact.repetition
                   + offense.sub.rep.GW.2
                   + offense.closure
                   + offense.degree
                   + strata(stratum)
                   , robust = TRUE
                   , data = events.meetings
                   , control = coxph.control(iter.max = 40)
)
summary(meetings2) # meeting.sub.rep.GW.2 NOT AVAILABLE

# checking multicollinearity
library(rms)

meetVIF <- vif(meetings1)
meetVIF 

# reduced model w/ closure as the only structural effect
meetings.redux <- coxph(Surv(time = rep(1,nrow(events.meetings)), 
                             event = events.meetings$IS_OBSERVED) ~
                          ratio.leader
                        + diff.leader
                        + avg.age
                        + diff.age
                        + diff.locale
                        + kin
                        + meeting.closure
                        + offense.closure
                        + call.closure
                        + strata(stratum)
                        , robust = TRUE
                        , data = events.meetings
                        , control = coxph.control(iter.max = 40)
)
summary(meetings.redux)


#### calls network as the dependent variable
events.calls <- read.csv("InfinitoEvents_Revise2_CALLS.csv")

sum(events.calls$IS_OBSERVED) # 383 call events /// R1 383, R2 381
summary(events.calls$size) # min 2, med 2, mean 2.008, max 3/// R1 same
hist(events.calls$size, breaks = c(0:25)) # size histogram
summary(events.calls) # summary stats of all instances
summary(events.calls[events.calls$IS_OBSERVED == 1,]) # summary of events
summary(events.calls[events.calls$IS_OBSERVED == 0,]) # summary of non-events

names(events.calls)
# square root transformation of all endogenous effects (network effects based on prior events)
events.calls[,c(22:ncol(events.calls))] <- sqrt(events.calls[,c(22:ncol(events.calls))])

# strata are identified by TIME & SIZE (simultaneous events on the same day)
events.calls$stratum <- paste(events.calls$TIME,":", events.calls$size)

calls1 <- coxph(Surv(time = rep(1,nrow(events.calls)), 
                        event = events.calls$IS_OBSERVED) ~
                     ratio.leader
                   + diff.leader
                   + avg.age
                   + diff.age
                   + diff.locale
                   + kin
                   + meeting.exact.repetition 
                   + meeting.sub.rep.GW.5
                   + meeting.closure
                   + meeting.degree
                   + call.exact.repetition
                   + call.sub.rep.GW.5
                   + call.closure
                   + call.degree
                   + offense.exact.repetition
                   + offense.sub.rep.GW.5
                   + offense.closure
                   + offense.degree
                   + strata(stratum)
                   , robust = TRUE
                   , data = events.calls
                   , control = coxph.control(iter.max = 40)
)
summary(calls1)

calls2 <- coxph(Surv(time = rep(1,nrow(events.calls)), 
                     event = events.calls$IS_OBSERVED) ~
                  ratio.leader
                + diff.leader
                + avg.age
                + diff.age
                + diff.locale
                + kin
                + meeting.exact.repetition 
                + meeting.sub.rep.GW.2
                + meeting.closure
                + meeting.degree
                + call.exact.repetition
                + call.sub.rep.GW.2
                + call.closure
                + call.degree
                + offense.exact.repetition
                + offense.sub.rep.GW.2
                + offense.closure
                + offense.degree
                + strata(stratum)
                , robust = TRUE
                , data = events.calls
                , control = coxph.control(iter.max = 40)
)
summary(calls2)

callVIF <- vif(calls1)
callVIF # extreme on calls (degree and subrep1), high on meeting rep, degree bad

# re-fitting the model without exact repetition
calls3 <- coxph(Surv(time = rep(1,nrow(events.calls)), 
                     event = events.calls$IS_OBSERVED) ~
                  ratio.leader
                + diff.leader
                + avg.age
                + diff.age
                + diff.locale
                + kin
                + meeting.exact.repetition 
                + meeting.sub.rep.GW.5
                + meeting.closure
                + meeting.degree
                #+ call.exact.repetition
                + call.sub.rep.GW.5
                + call.closure
                + call.degree
                + offense.exact.repetition
                + offense.sub.rep.GW.5
                + offense.closure
                + offense.degree
                + strata(stratum)
                , robust = TRUE
                , data = events.calls
                , control = coxph.control(iter.max = 40)
)
summary(calls3)

callVIF2 <- vif(calls2)
callVIF2 # improvement

# reduced model w/ closure as the only structural effect
calls.redux <- coxph(Surv(time = rep(1,nrow(events.calls)), 
                          event = events.calls$IS_OBSERVED) ~
                       ratio.leader
                     + diff.leader
                     + avg.age
                     + diff.age
                     + diff.locale
                     + kin
                     + meeting.closure
                     + offense.closure
                     + call.closure
                     + strata(stratum)
                     , robust = TRUE
                     , data = events.calls
                     , control = coxph.control(iter.max = 40)
)
summary(calls.redux)

#### offending network as the dependent variable
events.offenses <- read.csv("InfinitoEvents_Revise2_OFFENSES.csv")

sum(events.offenses$IS_OBSERVED) # 98 offence events /// R1: 224, R2: 116
summary(events.offenses$size) # min 1, med 3, mean 3.71, max 11// R1&R2 diff mean
hist(events.offenses$size, breaks = c(0:11)) # size histogram
summary(events.offenses) # summary stats of all instances
summary(events.offenses[events.offenses$IS_OBSERVED == 1,]) # summary of events
summary(events.offenses[events.offenses$IS_OBSERVED == 0,]) # summary of non-events

names(events.offenses)
# square root transformation of all endogenous effects (network effects based on prior events)
events.offenses[,c(22:ncol(events.offenses))] <- sqrt(events.offenses[,c(22:ncol(events.offenses))])

# strata are identified by TIME & SIZE (simultaneous events on the same day)
events.offenses$stratum <- paste(events.offenses$TIME,":", events.offenses$size)

offenses1 <- coxph(Surv(time = rep(1,nrow(events.offenses)), 
                     event = events.offenses$IS_OBSERVED) ~
                  ratio.leader
                + diff.leader
                + avg.age
                + diff.age
                + diff.locale
                + kin
                + meeting.exact.repetition 
                + meeting.sub.rep.GW.5
                + meeting.closure
                + meeting.degree
                + call.exact.repetition
                + call.sub.rep.GW.5
                + call.closure
                + call.degree
                + offense.exact.repetition
                + offense.sub.rep.GW.5
                + offense.closure
                + offense.degree
                + strata(stratum)
                , robust = TRUE
                , data = events.offenses
                , control = coxph.control(iter.max = 40)
)
summary(offenses1)

offenses2 <- coxph(Surv(time = rep(1,nrow(events.offenses)), 
                        event = events.offenses$IS_OBSERVED) ~
                     ratio.leader
                   + diff.leader
                   + avg.age
                   + diff.age
                   + diff.locale
                   + kin
                   + meeting.exact.repetition 
                   + meeting.sub.rep.GW.2
                   + meeting.closure
                   + meeting.degree
                   #+ call.exact.repetition
                   + call.sub.rep.GW.2
                   + call.closure
                   + call.degree
                   + offense.exact.repetition
                   + offense.sub.rep.GW.2
                   + offense.closure
                   + offense.degree
                   + strata(stratum)
                   , robust = TRUE
                   , data = events.offenses
                   , control = coxph.control(iter.max = 40)
)
summary(offenses2)

# re-fitting the model without exact repetition
offenses3 <- coxph(Surv(time = rep(1,nrow(events.offenses)), 
                        event = events.offenses$IS_OBSERVED) ~
                     ratio.leader
                   + diff.leader
                   + avg.age
                   + diff.age
                   + diff.locale
                   + kin
                   + meeting.exact.repetition 
                   + meeting.sub.rep.GW.5
                   + meeting.closure
                   + meeting.degree
                   #+ call.exact.repetition
                   + call.sub.rep.GW.5
                   + call.closure
                   + call.degree
                   + offense.exact.repetition
                   + offense.sub.rep.GW.5
                   + offense.closure
                   + offense.degree
                   + strata(stratum)
                   , robust = TRUE
                   , data = events.offenses
                   , control = coxph.control(iter.max = 40)
)
summary(offenses3)

offVIF <- vif(offenses1)
offVIF 

# reduced model w/ closure as the only structural effect
offenses.redux <- coxph(Surv(time = rep(1,nrow(events.offenses)), 
                             event = events.offenses$IS_OBSERVED) ~
                          ratio.leader
                        + diff.leader
                        + avg.age
                        + diff.age
                        + diff.locale
                        + kin
                        + meeting.closure
                        + offense.closure
                        + call.closure
                        + strata(stratum)
                        , robust = TRUE
                        , data = events.offenses
                        , control = coxph.control(iter.max = 40)
)
summary(offenses.redux)
