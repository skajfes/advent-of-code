
param(
	[Parameter(Mandatory=$true)]
	[int]$Year,
	[Parameter(Mandatory=$true)]
	[int]$Day
)

pushd $PSScriptRoot


$session = $env:AOC_SESSION
$p = "Day$($Day.ToString("00"))"

if ($session -eq $null) {
	Write-Host "AOC_SESSION env variable is not set"
	return
}

# download the file
curl -HGET https://adventofcode.com/$Year/day/$Day/input -H "Cookie: session=$session" > $Year\$p\input.txt

$name = Get-ChildItem $Year\$p\$p.fsproj | %{ $_.FullName }

if ((sls -Path $name -Pattern "input.txt").Length -eq 0) {
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
}

cat $name
cat $Year\$p\input.txt

popd
