display/i $pc
display/x $edx
display/x $ecx
display/x $ebx
display/x $eax
display/32wx $ebp-92
display/32wx $esp
display/s $edx
display/s $ecx
display/s $ebx
display/s $eax
display/s $ebp-92
display/s $esp

define regpo
  po $eax
  po $ebx
  po $ecx
  po $edx
  po $ebp-92
  po $esp
end

document regpo
  objC print-object display registers
end
