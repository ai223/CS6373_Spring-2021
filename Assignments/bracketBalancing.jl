function balanceBrackets(input::String)
  brackets = []
  partners = Dict(']' => '[', ')' => '(', '}' => '{')

  for c in input
    if c == '[' || c == '{' || c == '(' push!(brackets, c) end
    if c == ']' || c == '}' || c == ')'
      previousC = isempty(brackets) ? ' ' : pop!(brackets)
      expectedC = partners[c]
      if (previousC != expectedC) return false end;
    end
  end

  return isempty(brackets);
end

println(balanceBrackets("hello world"))
println(balanceBrackets("()(){}[]"))
println(balanceBrackets("()([]"))
println(balanceBrackets("()()}[]"))
println(balanceBrackets("("))
println(balanceBrackets(")"))
println(balanceBrackets("([{}({}[[{(())}]])])"))
