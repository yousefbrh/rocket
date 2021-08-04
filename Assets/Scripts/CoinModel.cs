using System;
using UnityEngine;

public class CoinModel : MonoBehaviour
{
    public ParticleSystem particle;
    public MeshRenderer meshrenderer;
    public Collider collider;
    public Light light;



    private void OnTriggerEnter(Collider other)
    {
        if (!other.CompareTag("Friendly")) return;

        CurrencyManager.instance.Earn(1);
        particle.Play();
        meshrenderer.enabled = false;
        collider.enabled = false;
        light.gameObject.SetActive(false);
        Destroy(gameObject, particle.main.duration);

    }
}