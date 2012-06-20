function printf(format, ...)
	print(format:format(...))
end
	
function dofile(filename, ...)
	_ENV.arg = {[0] = filename, ...}
	return assert(loadfile(filename))(...)
end

local utils = {}

function utils.switch(c)
	local swtbl = {
		casevar = c,
		caseof = function (self, code)
			local f
			if self.casevar then
				f = code[self.casevar] or code.default
			else
				f = code.missing or code.default
			end
			if f then
				if type(f)=="function" then
					return f()
				elseif type(f) == 'table' then
					return f[1](table.unpack(f, 2, #f))
				elseif type(f) == 'string' then
					error(string.format(f, self.casevar))
				else
					error("invalid case "..tostring(self.casevar))
				end
			end
		end
	}
	return swtbl
end

function utils.invtable(t)
	local ret = {}
	for k,v in pairs(t) do
		ret[v] = k
	end
	return ret
end

function utils.array_concat(...)
	local res, arg = {}, {...}
	local function insert(val)
		if type(val) == 'table' then
			for k,v in pairs(val) do
				if type(k) == 'number' then
					insert(v)
				else
					res[k] = v
				end
			end
		else
			table.insert(res, val)
		end
	end
	insert(arg)
	return res
end

function utils.keys(t, fsort)
	local res = {}
	local oktypes = { string = true, number = true }
	local function cmpfct(a,b)
		if type(a) == type(b) and oktypes[type(a)] then
			return a < b
		else
			return type(a) < type(b)
		end
	end
	for k in pairs(t) do
		res[#res+1] = k
	end
	if fsort then
		table.sort(res, cmpfct)
	end
	return res
end

function utils.get(data)
	if #data > 260 then return data end
	local prefix,name = data:match('^([A-Z][A-Z][A-Z][A-Z]?[A-Z]?):"?(.-)"?$')
	if prefix == 'FILE' then
		local file = assert(io.open(name, 'rb'))
		data = file:read("*a")
		file:close()
	elseif prefix == 'RAW' then
		data = name
	elseif #data > 10 and data:match("[%/%\\_%w%: %.]+%.%w%w%w+$") and lfs.attributes(data) then
		printf("Warning: The string '%s' is a valid file name, but will be interpreted as raw data. Did you forget the 'FILE:' prefix?", data)
	end
	return data
end

function utils.put(data, filename)
	local prefix,name = filename:match('^([A-Z][A-Z][A-Z][A-Z]?[A-Z]?):(.*)')
	local file
	if prefix == 'FILE' then
		filename = name
		file = assert(io.open(filename, 'wb'))
		file:write(utils.get(data))
		file:close()
	end
end

function utils.rformat(little_endian, str, format, ftableret)
	local idx = 0
	local res = {}
	local i = 1
	while i <= #format do
		local signed
		if format:sub(i,i) == '-' then
			signed = true
			i = i + 1
		end
		local size = tonumber(format:sub(i,i))
		i = i + 1
		local val = str:sub(idx+1,idx+size)
		local value = 0
		idx = idx + size
		if little_endian then
			val = string.reverse(val)
		end
		for j=1,size do
			value = value * 256 + val:byte(j)
		end
		if signed and value >= 2^(size*8-1) then
			value = value - 2^(size*8)
		end
		res[#res+1] = value
	end
	if ftableret then
		return res
	else
		return table.unpack(res)
	end
end

function utils.wformat(little_endian, format, ...)
	local res = {}
	local values = utils.array_concat(...)
	format = format:gsub("-", "")
	for i=1,#format do
		local size = tonumber(format:sub(i,i))
		local value = values[i]
		local str = ""
		for j=1,size do
			str = str .. string.char(value % 256)
			value = math.floor(value / 256)
		end
		if not little_endian then
			str = string.reverse(str)
		end
		res[#res+1] = str
	end
	return table.concat(res)
end

-- To use, assign: new.debugcb = utils.debugcb
function utils.debugcb(class, ...)
	local i=0
	printf("new %s(%s", class, select('#', ...) == 0 and ')' or '')
	for type,param,default in new.prototype[class]:gmatch("(%S+) (%S+) (%S*) ") do
		i = i + 1
		local val = select(i, ...)
		if val == nil then
			val = string.format("[%s]", default)
		else
			val = utils.dumplua(val, '', "t3f1u1", 1)
		end
		printf("  %s %s = %s%s", type, param, val, i==select('#', ...) and ')' or ',')
	end
end

function utils.base(base)
	return setmetatable({base = base}, {__index = function(t,k)
		return assert(tonumber(k:gsub("_",""), t.base), "Invalid number format")
	end})
end

function utils.dumplocal(limit, fastmode)
	limit = limit or 200
	print("\nDumping variables down the stack:")
	for level=2,math.huge do
		local info = debug.getinfo(level)
		if not info then break end
		local name, line = "", info.short_src
		if info.linedefined > 0 then
			line = line .. ":" .. info.currentline
		end
		if info.namewhat ~= "" then
			name = info.namewhat..' "'..info.name..'" '
		end
		name = string.format("Level %d: %s(%s)\n", level, name, line)
		for j=1,2 do
			for i=1, math.huge do
				local n, v, dump, res
				if j == 1 then
					n, v = debug.getlocal(level, i)
				else
					n, v = debug.getupvalue(info.func or function() end, i)
				end
				if not n then break end
				if fastmode then
					dump = tostring(v):sub(1,limit)
				else
					res, dump = pcall(utils.dumplua, v, '', 'u1h1', 4)
					if not res then
						dump = tostring(v)
					end
				end
				dump = dump:gsub("[%z\1-\8\12-\31]", function(c) return string.format("\\%03d", string.byte(c)) end)
				if #dump > limit then
					dump = dump:sub(1,limit) .. " ..."
				end
				if i==1 then
					print(string.format("%s  %s variables:", name, ({"Local", "Non-local"})[j]))
					name = ""
				end
				print(string.format('    %d: %s = %s', i, n, dump))
			end
		end
	end
end

function utils.trace(filename, fct, ...)
	local f = assert(io.open(filename, "w"))
	local level = 1
	local whatlut = { global="", ["local"]="", upvalue="", field="?.", method="?:" }
	local col = 0
	local function hook(what, line)
		local info = debug.getinfo(2)
		if what == "line" then
			if col > 12 then f:write "\n" col = 0 end
			if col == 0 then f:write(string.rep("  ", level)) end
			col = col + 1
			f:write("[",line, "] ")
			return
		end
		if col > 0 then f:write "\n" end
		col = 0
		if what == "call" then
			local name, line = "?", " "
			if info.linedefined > 0 then
				line = " ["..info.short_src .. ":" .. info.linedefined.."]"
			end
			if info.namewhat ~= "" then
				name = whatlut[info.namewhat]..info.name
			end

			f:write(string.rep("  ", level), "Call ", name, " (")
			for i=1, math.huge do
				local n, v = debug.getlocal(2, i)
				if v == nil then break end
				if type(v) == 'string' then
					v = '"'.. v:gsub("%c","."):sub(1, 30) ..'"'..#v
				else
					v = tostring(v)
				end
				if i > 1 then f:write ", " end
				if n:sub(1,1) ~= "(" then f:write(n, " = ") end
				f:write(v)
			end
			f:write(") ", line, "\n")
			level = level + 1
		else
			level = level - 1
			f:write(string.rep("  ", level), "Return ", (info.name or ""), "\n")
		end
	end
	debug.sethook(hook, "crl")
	local res = { pcall(fct, ...) }
	debug.sethook()
	f:close()
	assert(res[1], res[2])
	return table.unpack(res, 2)
end

function utils.dump(t, level, ident, defined)
	level = level or 20
	ident = ident or 0
	defined = defined or {}
	if level <= 0 then return end
	local tabs = string.rep("  ", ident)
	if defined[t] then
		print(tabs.."(Loop detected)")
		return
	end
	defined[t] = true
	for key,value in pairs(t) do
		if type(value) == 'table' then
			print(tabs..key..": [Table]")
			utils.dump(value, level-1, ident+1, defined)
		else
			print(tabs..key..": "..tostring(value))
		end
	end
	defined[t] = nil
end

local dumplua_closure = [[
local closures = {}
local function closure(t)
	closures[#closures+1] = t
	t[1] = assert(load(t[1]))
	return t[1]
end

for _,t in pairs(closures) do
	for i = 2,#t do
		debug.setupvalue(t[1], i-1, t[i])
	end
end
]]
local c_functions = {}
local lua_reserved_keywords = {
	'and', 'break', 'do', 'else', 'elseif', 'end', 'false', 'for',
	'function', 'if', 'in', 'local', 'nil', 'not', 'or', 'repeat',
	'return', 'then', 'true', 'until', 'while' }

for _,lib in pairs{'_G', 'string', 'table', 'math', 'readline', 'history', 'graph',
	'io', 'os', 'coroutine', 'package', 'debug', 'bit', 'zip', 'unzip', 'lfs'} do
	local t = _G[lib] or {}
	lib = lib .. "."
	if lib == "_G." then lib = "" end
	for k,v in pairs(t) do
		if type(v) == 'function' then -- and not pcall(string.dump, v) then
			c_functions[v] = lib..k
		end
	end
end

function utils.dumplua(value, varname, mode, ident)
	local type_lut = { t = 'table', s = 'string', n = 'number', b = 'boolean', 
		l = 'nil', f = 'function', u = 'userdata', h = 'thread' }
	-- Local variables for speed optimization
	local string_format, type, string_dump, string_rep, tostring, pairs, table_concat =
	      string.format, type, string.dump, string.rep, tostring, pairs, table.concat
	local keycache, strvalcache, out, closure_cnt = {}, {}, {}, 0
	local defined, fcts, dumplua = {}, {}, nil
	setmetatable(strvalcache, {__index = function(t,value)
		local res = string_format('%q', value)
		t[value] = res
		return res
	end})
	for k,v in pairs(type_lut) do
		fcts[k..0] = function() error(string_format("Cannot dump %ss", v)) end
		fcts[k..1] = function(value) return tostring(value) end
	end
	fcts.s2 = function(value) return strvalcache[value] end
	fcts.f2 = function(value) return string_format("load(%q)", string_dump(value)) end

	local function test_defined(value, path)
		if defined[value] then
			if path:match("^getmetatable.*%)$") then
				out[#out+1] = string_format("s%s, %s)\n", path:sub(2,-2), defined[value])
			else
				out[#out+1] = path .. " = " .. defined[value] .. "\n"
			end
			return true
		end
		defined[value] = path
	end
	local function make_key(t, key)
		local s
		if type(key) == 'string' and key:match('^[_%a][_%w]*$') then
			s = key .. "="
		else
			s = "[" .. dumplua(key, 0) .. "]="
		end
		t[key] = s
		return s
	end
	for _,k in ipairs(lua_reserved_keywords) do
		keycache[k] = '["'..k..'"] = '
	end
	fcts.t2 = function (value)
		-- Table value
		local numidx = 1
		out[#out+1] = "{"
		for key,val in pairs(value) do
			if key == numidx then
				numidx = numidx + 1
			else
				out[#out+1] = keycache[key]
			end
			local str = dumplua(val)
			out[#out+1] = str..","
		end
		out[#out+1] = "}"
		return ""
	end
	fcts.t3 = function (value, ident, path)
		if test_defined(value, path) then return "nil" end
		-- Table value
		local sep, str, numidx, totallen = " ", {}, 1, 0
		for _,key in pairs(utils.keys(value, true)) do
			local val = value[key]
			local s = ""
			local subpath = path
			if key == numidx then
				subpath = subpath .. "[" .. numidx .. "]"
				numidx = numidx + 1
			else
				s = keycache[key]
				if not s:match "^%[" then subpath = subpath .. "." end
				subpath = subpath .. s:gsub("%s*=%s*$","")
			end
			s = s .. dumplua(val, ident+1, subpath)
			str[#str+1] = s
			totallen = totallen + #s + 2
		end
		if totallen > 80 then
			sep = "\n" .. string_rep("  ", ident+1)
		end
		str = "{" .. sep .. table_concat(str, ","..sep) .. " " .. sep:sub(1,-3) .. "}"
		return str
	end
	fcts.t4 = function (value, ident, path)
		if test_defined(value, path) then return "nil" end
		-- Table value
		local sep, str, numidx, totallen = " ", {}, 1, 0
		local meta, metastr = (debug or _G).getmetatable(value)
		if meta then
			ident = ident + 1
			metastr = dumplua(meta, ident, "getmetatable("..path..")")
			totallen = totallen + #metastr + 16
		end
		for _,key in pairs(utils.keys(value, true)) do
			local val = value[key]
			local s = ""
			local subpath = path
			if key == numidx then
				subpath = subpath .. "[" .. numidx .. "]"
				numidx = numidx + 1
			else
				s = keycache[key]
				if not s:match "^%[" then subpath = subpath .. "." end
				subpath = subpath .. s:gsub("%s*=%s*$","")
			end
			s = s .. dumplua(val, ident+1, subpath)
			str[#str+1] = s
			totallen = totallen + #s + 2
		end
		if totallen > 80 then
			sep = "\n" .. string_rep("  ", ident+1)
		end
		str = "{" .. sep .. table_concat(str, ","..sep) .. " " .. sep:sub(1,-3) .. "}"
		if meta then
			sep = sep:sub(1,-3)
			return "setmetatable(" .. sep .. str .. "," .. sep .. metastr .. sep:sub(1,-3) .. ")"
		end
		return str
	end
	fcts.f3 = function (value, ident, path)
		if test_defined(value, path) then return "nil" end
		if c_functions[value] then
			return c_functions[value]
		elseif debug.getupvalue(value, 1) == nil then
			return string_format("load(%q)", string_dump(value))
		end
		closure_cnt = closure_cnt + 1
		local res = {string.dump(value)}
		for i = debug and 1 or math.huge,1e9 do
			local name, v = debug.getupvalue(value,i)
			if name == nil then break end
			res[i+1] = v
		end
		return "closure " .. dumplua(res, ident, "closures["..closure_cnt.."]")
	end

	mode = "t4s2n1b1l1f3u0h0g2" .. (mode or "")
	for s,l in mode:gmatch("(([tsnblfuh])%d)") do
		fcts[type_lut[l]] = fcts[s]
	end

	function dumplua(value, ident, path)
		return fcts[type(value)](value, ident, path)
	end
	if varname == nil then
		varname = "return "
	elseif varname:match("^[%a_][%w_]*$") then
		varname = varname .. " = "
	end
	if mode:match("g1") then
		setmetatable(keycache, {__index = make_key })
		out[1] = varname
		table.insert(out,dumplua(value, 0))
		return table_concat(out)
	else
		setmetatable(keycache, {__index = make_key })
		local items = {}
		for i=1,10 do items[i] = '' end
		items[3] = dumplua(value, ident or 0, "t")
		if closure_cnt > 0 then
			items[1], items[6] = dumplua_closure:match("(.*\n)\n(.*)")
			out[#out+1] = ""
		end
		if #out > 0 then
			items[2] = "local t = "
			items[4] = "\n"
			items[5] = table_concat(out)
			items[7] = varname .. "t"
		else
			items[2] = varname
		end
		return table_concat(items)
	end
end

function utils.hexdump(data)
	local str, format, a, b
	if type(data) == 'string' then
		for i=1,#data,16 do
			str = data:sub(i,i+15)
			format = string.rep("%02X ", #str)
			a = format:format(str:byte(1,-1))
			b = str:gsub("[\1-\31%z]", ".")
			printf("%04X   %-24s  %-24s  %-16s", i-1, a:sub(1,24), a:sub(25,-1), b)
		end
	elseif type(data) == 'table' then
		local m, nibbles, nb_line = 0, 8, 4
		for i=1,#data do
			m = math.max(m, math.abs(data[i]))
		end
		if m < 2^8 then
			nibbles, nb_line = 2, 16
		elseif m < 2^16 then
			nibbles, nb_line = 4, 8
		end
		for i=1,#data,nb_line do
			str = { table.unpack(data, i, i+nb_line-1) }
			format = string.rep("%0"..nibbles.."X ", #str)
			a = format:format(table.unpack(str))
			printf("%04X   %s", i-1, a)
		end
	end
end


local newid = 1
function utils.connect(root, nodes)
	local rdirs = { root:GetBufferDirection() }
	assert(#rdirs == #nodes)
	for i=1,#nodes do
		local node=nodes[i]
		local ndirs = { node:GetBufferDirection() }
		for j=1,#ndirs do
			local id = node:GetOneBufferId(j-1)
			if rdirs[i] ~= ndirs[j] and id == 0 then
				node:SetOneBufferId(j-1, newid)
				root:SetOneBufferId(i-1, newid)
				newid = newid + 1
				break
			end
		end
	end
end

function utils.preconnect(proc, rootidx, nodesidx)
	local function abs(idx)
		if idx > 0 then return idx end
		return #proc + idx + 1
	end
	local c = proc.connections or {}
	proc.connections = c
	c[#c+1] = {root = abs(rootidx)}
	for i=1,#nodesidx do
		c[#c][i] = abs(nodesidx[i])
	end
	utils.dump(c)
end

function utils.make_connections(proc)
	local c = proc.connections or {}
	for i=1,#c do
		local t = {}
		for j=1,#c[i] do
			t[j] = proc[c[i][j]]
		end
		utils.connect(proc[c[i].root], t)
	end
end

utils.complete_table = {
	[collectgarbage] = {{"stop","restart","collect","count","step","setpause","setstepmul"}},
	[require or ''] = {function()
		local res = {}
		for p in package.path:gmatch('[^%;]+') do repeat
			local dir,remain = p:match("(.+)[\\/]%?(.*)")
			if not dir then break end
			for f in lfs.dir(dir) do
				res[#res+1] = f:match("(.*)"..remain)
				if lfs.attributes(dir.."/"..f).mode == 'directory' and remain:match("^[\\/]") then
					local file = io.open(dir.."/"..f..remain, "r")
					if file then
						res[#res+1] = f
						file:close()
					end
				end
			end
		until true end
		return res
		end},
}
setmetatable(utils.complete_table, {__mode = 'k'})

function utils.completion(word, line, startpos, endpos)
	local matches = { }
	local function add(...)
		local list = utils.array_concat(...)
		for _,value in pairs(list) do
			value = tostring(value)
			if value:match("^"..word) then
				matches[#matches+1] = value
			end
		end
	end

	local function filename_list(str)
		local path, name = str:match("(.*)[\\/]+(.*)")
		path = (path or ".").."/"
		path = path:gsub("^FILE:","")
		name = name or str
		for f in lfs.dir(path) do
			if (lfs.attributes(path..f) or {}).mode == 'directory' then
				add(f.."/")
			else
				add(f)
			end
		end
	end

	local function postfix(v)
		local t = type(v)
		if t == 'function' or (getmetatable(v) or {}).__call then return '('
		elseif t == 'table' and #v > 0 then	return '['
		elseif t == 'table' then return '.'
		elseif t == 'userdata' then	return ':'
		else return ' '
		end
	end

	local function add_globals()
		for _,k in ipairs(lua_reserved_keywords) do
			add(k..' ')
		end
		for k,v in pairs(_G) do
			add(k..postfix(v))
		end
	end
	local function add_fields(t, sep)
		local wanted, ending = 'string', ''
		if sep == '[' then
			wanted, ending = 'number', ']'
		end
		if type(t) == 'table' then
			if t == option then  
				add('get(', 'list[')
				return add_fields(utils.invtable(option.list), sep) 
			end
			for k,v2 in pairs(t) do
				if type(k) == wanted then add(k..ending..postfix(v2)) end
			end
		end
		t = (getmetatable(t) or {}).__index
		if type(t) == 'table' then
			for k,v2 in pairs(t) do
				if type(k) == wanted and not tostring(k):match('^__') then
					add(k..ending..postfix(v2))
				end
			end
		end
	end

	local function vcall_param(v, param_idx, str)
		for _,i in pairs(hsf.Class[v.object.class][v.fctname]) do
			local t = (i.Parameters[param_idx] or {})[1]
			if t == 'CNotifier*' or t == 'CMotorNotifier*'then
				add('auto', 'out')
			elseif hsf.Enum[t] then
				for j,_ in pairs(hsf.Enum[t]) do
					add(j)
				end
			elseif str then
				filename_list(str)
			end
		end
	end

	local function lua_prototype(expr, fct)
		local info, src = debug.getinfo(fct, 'S')
		if info.source == "@Pipeline" then
			src = new.luacode
		elseif info.source:sub(1,1) == '@' then
			src = utils.get("FILE:"..info.source:sub(2))
		end
		if src then
			src = src:match(string.rep(".-\n", info.linedefined-1).."%s*function%s+(.-%))")
		end
		if src then return src end
		src = utils.get("FILE:P:/Yminds/sft/LuaDura/Doc/Lua5.1/manual.html")
		local pat = expr
		pat = pat:gsub("(%W)","%%%1")
		pat = '<hr><h3><a name="pdf%-'..pat..'"><code>(.-)</code></a></h3>'
		src = src:match(pat)
		if src then
			src = src:gsub("&middot;",".")
			return src
		end
	end
	
	local function known_function(outofstring, prototype, param_idx, prefix)
		if outofstring then
			local t = {}
			for type,param,default in prototype:gmatch("(%S+) (%S+) (%S*) ") do
				t[#t+1] = string.format("%s %s%s", type, param, #default>0 and "="..default or "")
			end
			if param_idx and param_idx <= #t then
				t[param_idx] = string.format("*** %s ***", t[param_idx])
			end
			local proto = string.format("%s(%s)", prefix, table.concat(t, ', '))
			add("~", proto)
		else
			local type = prototype:match(("%S+ %S+ %S* "):rep((param_idx or 1)-1).."(%S+)")
			if new.enum[type] then
				add(utils.keys(new.enum[type]))
			end
		end
	end

	local function contextual_list(expr, sep, str, param_idx)
		if expr == nil or expr == "" then return add_globals() end
		local v = load("return "..expr:gsub(":","."))
		if not v then return end
		v = v()
		local t = type(v)
		if sep == '.' or sep == ':' then add_fields(v, sep)
		elseif sep == '[' then
			add_fields(v, sep)
			if word ~= "" then return add_globals() end
		elseif sep == '(' then
			local class = expr:match("new.(C%w+)")
			local obj, fct = expr:match("(%w+)%:(%w+)")
			if class then
				return known_function(not str and word == '', new.prototype[class], param_idx, "new."..class)
			elseif fct then
				v = load("return "..obj..":GetType()")
				if v then t = v() end
				while t do
					if new.method[t][fct] then
						return known_function(not str and word == '', new.method[t][fct], param_idx, ("%s:%s"):format(obj, fct))
					end
					t = new.base[t]
				end
			elseif t == 'function' then
				if str and utils.complete_table[v] and utils.complete_table[v][param_idx] then
					local l = utils.complete_table[v][param_idx]
					if type(l) == 'function' then l = l() end
					return add(l)
				end
				local proto, arg = lua_prototype(expr, v)
				if proto and param_idx then 
					local args = proto:gsub("[ %[%]]",""):match("%((.*)%)") or ""
					arg = args:match((".-,"):rep(param_idx-1).."([^,]+)")
				end
				if str and arg == 'data' and not str:match(":") then return add('FILE:a', 'FILE:b') end
				if str then return filename_list(str) end
				if word ~= '' then return add_globals() end
				if proto then return add("~", proto) end
			end
		end
	end

	local function simplify_expression(expr)
		expr = expr:gsub("\\(['\"])", function(c) return
			string.format("\\%03d", string.byte(c)) end)
		local curstring
		-- remove (finished and unfinished) literal strings
		while true do
			local idx1,_,equals = expr:find("%[(=*)%[")
			local idx2,_,sign = expr:find("(['\"])")
			if idx1 == nil and idx2 == nil then break end
			local idx,startpat,endpat
			if (idx1 or math.huge) < (idx2 or math.huge) then
				idx,startpat,endpat  = idx1, "%["..equals.."%[", "%]"..equals.."%]"
			else
				idx,startpat,endpat = idx2, sign, sign
			end
			if expr:sub(idx):find("^"..startpat..".-"..endpat) then
				expr = expr:gsub(startpat.."(.-)"..endpat, " STRING ")
			else
				expr = expr:gsub(startpat.."(.*)", function(str)
					curstring = str; return "(CURSTRING " end)
			end
		end
		expr = expr:gsub("%b()"," PAREN ")      -- remove table constructors
		expr = expr:gsub("%b{}"," TABLE ")      -- remove groups of parentheses
		-- avoid two consecutive words without operator
		expr = expr:gsub("(%w)%s+(%w)","%1|%2")
		expr = expr:gsub("%s","")               -- remove now useless spaces
		local before, sep = expr:match("([%.%:%w%[%]_]-)([%.%:%[%(])"..word.."$")
		if before then return before, sep, curstring end
		local param, param_nb
		before, sep, param = expr:match("([%.%:%w%[%]_]-)(%()(.*)$")
		if param then
			param = param:gsub("[^,]","")
			param_nb = #param+1
		end
		return before, sep, curstring, param_nb
	end

	local expr, sep, str, param_nb = simplify_expression(line:sub(1,endpos))
	contextual_list(expr, sep, str, param_nb)
	return matches
end

return utils
