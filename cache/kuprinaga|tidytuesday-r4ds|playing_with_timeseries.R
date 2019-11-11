
inds <- seq(min(data_for_plot$date), 
            max(data_for_plot$date), 
            by = "day")

tseries <- ts(data_for_plot$weekly_active_members,
              start = c(1, 1),
              frequency = 7)

plot(tseries)

stl_object <- stl(tseries, s.window='periodic')

autoplot(stl_object)