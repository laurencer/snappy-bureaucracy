wrk.port = 8000
wrk.host = "localhost"
wrk.method = "POST"

local file = io.open("message.bin", "rb")
wrk.body = file:read("*all")
file:close()
print ("loaded file")

init = function (args)
   wrk.init(args)
end

