// 24.4
type TimeOfDay = { hours: int; minutes: int; f: string }

let (.>.) x y = x.f > y.f || x.f = y.f && (x.hours = y.hours && x.minutes > y.minutes || x.hours <> y.hours && (x.hours = 12 || y.hours <> 12 && x.hours > y.hours))