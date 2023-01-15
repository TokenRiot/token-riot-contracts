def changeRefHash(oldPath, newPath, newText):
    pattern = 'refValidatorHash = PlutusV2.ValidatorHash $ createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create the new addition
                    line = pattern + " " + str(newText) + "\n"
                file2.write(line)

def changeStarterPid(oldPath, newPath, newText):
    pattern = 'starterPid = PlutusV2.CurrencySymbol {PlutusV2.unCurrencySymbol = createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create the new addition
                    line = pattern + " " + str(newText) + " }\n"
                file2.write(line)

def changeStarterTkn(oldPath, newPath, newText):
    pattern = 'starterTkn = PlutusV2.TokenName {PlutusV2.unTokenName = createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create the new addition
                    line = pattern + " " + str(newText) + " }\n"
                file2.write(line)