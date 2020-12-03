
param(
	[Parameter(Mandatory=$true)]
	[int]$Year,
	[Parameter(Mandatory=$true)]
	[int]$Day
)

pushd $PSScriptRoot

$session = $env:AOC_SESSION
$p = "Day$($Day.ToString("00"))"

# download the file
curl -HGET https://adventofcode.com/$Year/day/$Day/input -H "Cookie: session=$session" > $Year\$p\input.txt

$name = Get-ChildItem $Year\$p\$p.fsproj | %{ $_.FullName }
$file = New-Object XML
$file.Load($name)
$content = $file.CreateElement("Content")
$includeAttr = $file.CreateAttribute("Include")
$includeAttr.Value = "input.txt"
$copy = $file.CreateElement("CopyToOutputDirectory")
$copy.InnerText = "PreserveNewest"
$content.AppendChild($copy)
$content.Attributes.Append($includeAttr)
$file.Project.ItemGroup.AppendChild($content)
$file.save($name)

popd
