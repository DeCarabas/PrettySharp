class Foo
{
    public void Dispose()
    {
        var blendedTokens = _blendedTokens;
        if (blendedTokens != null)
        {
            _blendedTokens = null;
            // Used to have a bug: "Expected an identifier in type name"
            if (blendedTokens.Length < 4096)
            {
                Array.Clear(blendedTokens, 0, blendedTokens.Length);
                s_blendedNodesPool.Free(blendedTokens);
            }
            else
            {
                s_blendedNodesPool.ForgetTrackedObject(blendedTokens);
            }
        }
    }
}
