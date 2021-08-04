using System;
using DG.Tweening;
using UnityEngine;

public class CurrencyManager : MonoBehaviour
{
    public Action<int> OnCurrencyChange;

    public static CurrencyManager instance;

    private void Awake()
    {
        if (!instance) instance = this;
    }

    public void Earn(int value)
    {
        Prefs.playerCurrency += value;
        OnCurrencyChange?.Invoke(Prefs.playerCurrency);
    }

    public int GetCurrentValue()
    {
        return Prefs.playerCurrency;
    }
}