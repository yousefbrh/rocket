using UnityEngine;

public static class Prefs
{
    public static int playerLevel
    {
        set => PlayerPrefs.SetInt("CurrentLevel", value);
        get => PlayerPrefs.GetInt("CurrentLevel", 1);
    }
    
    public static int playerCurrency
    {
        set => PlayerPrefs.SetInt("CurrentCurrency", value);
        get => PlayerPrefs.GetInt("CurrentCurrency", 0);
    }
}