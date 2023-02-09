def changeLockHash(oldPath,newPath,newText):
    pattern = 'referenceHash = V2.ValidatorHash $ createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + "\n"
                file2.write(line)

def changeStartLockPid(oldPath,newPath,newText):
    pattern = 'lockPid = V2.CurrencySymbol {V2.unCurrencySymbol = createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + " }\n"
                file2.write(line)

def changeStartLockTkn(oldPath,newPath,newText):
    pattern = 'lockTkn = V2.TokenName {V2.unTokenName = createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern in line:
                    # create teh new addition
                    line = pattern + " " + str(newText) + " }\n"
                file2.write(line)


def changeMultiPkh(oldPath,newPath,newText1,newText2,newText3):
    pattern1 = 'multiPkh1 = V2.PubKeyHash { V2.getPubKeyHash = createBuiltinByteString'
    pattern2 = 'multiPkh2 = V2.PubKeyHash { V2.getPubKeyHash = createBuiltinByteString'
    pattern3 = 'multiPkh3 = V2.PubKeyHash { V2.getPubKeyHash = createBuiltinByteString'
    with open(oldPath, 'r') as file1:
        with open(newPath, 'w+') as file2:
            for line in file1:
                if pattern1 in line:
                    # create teh new addition
                    line = pattern1 + " " + str(newText1) + " }\n"
                if pattern2 in line:
                    # create teh new addition
                    line = pattern2 + " " + str(newText2) + " }\n"
                if pattern3 in line:
                    # create teh new addition
                    line = pattern3 + " " + str(newText3) + " }\n"
                file2.write(line)